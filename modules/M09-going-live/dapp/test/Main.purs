module Test.Main where

import Contract.Prelude
  ( ($)
  , (=<<)
  , (<$>)
  , (<>)
  , (+)
  , (-)
  , LogLevel (Trace)
  , Unit
  , flip
  , bind
  , pure
  , void
  , discard
  )

import Contract.Address as CA
import Contract.Wallet (withKeyWallet)
import Contract.Config (emptyHooks)
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipConfig
  , PlutipTest
  , testPlutipContracts
  , withWallets
  )
import Contract.Test.Utils
  ( ContractWrapAssertion
  , Labeled
  , exitCode
  , interruptOnSignal
  , withAssertions
  , assertLossAtAddress
  , assertGainAtAddress
  , label
  )
import Contract.Monad (Contract, liftedM)
import Data.Array (head)
import Data.BigInt as DBI
import Data.Maybe (Maybe (Just, Nothing))
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Seconds (Seconds))
import Data.UInt as DU
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff
  ( Milliseconds(Milliseconds)
  , cancelWith
  , effectCanceler
  , launchAff
  )
import Mote (test)
import Test.Spec.Runner (defaultConfig)

import Donation.Contract
  ( ContractResult
  , donation
  , reward
  )

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: DU.fromInt 8082
  , logLevel: Trace
  , ogmiosConfig:
      { port: DU.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: DU.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , kupoConfig:
      { port: DU.fromInt 1443
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: DU.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: emptyHooks
  , clusterConfig:
      { slotLength: Seconds 0.05 }
  }

type Address = Labeled CA.Address
type DonationParams = { donator :: Address }
type RewardParams = { visitor :: Address }

getOwnWalletLabeledAddress :: String -> forall a. Contract a Address
getOwnWalletLabeledAddress s = do
       addr <- liftedM ("Failed to get " <> s <> " address") $
         head <$> CA.getWalletAddresses
       pure $ label addr s

suite :: TestPlanM PlutipTest Unit
suite = do
  test "locks 10ADA to the contract" do
    let
      assertions :: DonationParams -> Array (ContractWrapAssertion () ContractResult)
      assertions { donator } =
        [ assertLossAtAddress donator
            \{ txFinalFee } -> pure $ DBI.fromInt 10_000_000 + txFinalFee
        ]
      distribution :: InitialUTxOs
      distribution =
        [ DBI.fromInt 20_000_000
        , DBI.fromInt 5_000_000
        ]
    withWallets distribution \kw -> withKeyWallet kw do
       donator <- getOwnWalletLabeledAddress "donator"
       void $ withAssertions (assertions { donator }) donation
  test "given the 42 answer, visitor gets all locked ADA from the contract" do
    let
      assertions :: RewardParams -> Array (ContractWrapAssertion () ContractResult)
      assertions { visitor } =
        [ assertGainAtAddress visitor
            \{ txFinalFee } -> pure $ DBI.fromInt 10_000_000 - txFinalFee
        ]
      distribution =
          [ DBI.fromInt 20_000_000
          , DBI.fromInt 5_000_000
          ]
        /\
          [ DBI.fromInt 1_000_000
          , DBI.fromInt 5_000_000
          ]
    withWallets distribution \(w1 /\ w2) -> do
       { txId: donationTxId } <- withKeyWallet w1 donation
       withKeyWallet w2 do
         visitor <- getOwnWalletLabeledAddress "visitor"
         void $ withAssertions (assertions { visitor }) (reward donationTxId)

main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true } $
      testPlutipContracts config suite

