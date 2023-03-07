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
import Contract.Test.Utils as CTU
import Contract.Monad 
  ( Contract
  , liftedM
  , liftContractM
  , liftContractE'
  )
import Contract.Scripts as CS
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
import Donation.Script (validator)

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

type Address = CTU.Labeled CA.Address
type Params = ( script :: Address )
type DonationParams = { donator :: Address | Params }
type RewardParams = { visitor :: Address | Params }

getOwnWalletLabeledAddress :: String -> forall a. Contract a Address
getOwnWalletLabeledAddress s = do
       addr <- liftedM ("Failed to get " <> s <> " address") $
         head <$> CA.getWalletAddresses
       pure $ CTU.label addr s

getScriptAddress :: forall a. Contract a Address
getScriptAddress = do
        validator' <- liftContractE' "Failed to parse validator" $
         CS.validatorHash <$> validator
        nId <- CA.getNetworkId
        valAddr <- liftContractM "Failed to get validator address" $
          CA.validatorHashEnterpriseAddress nId validator'
        pure $ CTU.label valAddr "script"

suite :: TestPlanM PlutipTest Unit
suite = do
  test "locks 10ADA to the contract" do
    let
      assertions :: DonationParams -> Array (CTU.ContractWrapAssertion () ContractResult)
      assertions { donator, script } = let amount = DBI.fromInt 10_000_000 in
        [ CTU.assertLossAtAddress donator
            \{ txFinalFee } -> pure $ amount + txFinalFee
        , CTU.assertGainAtAddress' script amount
        ]
      distribution :: InitialUTxOs
      distribution =
        [ DBI.fromInt 20_000_000
        , DBI.fromInt 5_000_000
        ]
    withWallets distribution \kw -> withKeyWallet kw do
       donator <- getOwnWalletLabeledAddress "donator"
       script <- getScriptAddress
       void $ CTU.withAssertions (assertions { donator, script }) donation
  test "given the 42 answer, visitor gets all locked ADA from the contract" do
    let
      assertions :: RewardParams -> Array (CTU.ContractWrapAssertion () ContractResult)
      assertions { visitor, script } = let amount = DBI.fromInt 10_000_000 in
        [ CTU.assertGainAtAddress visitor
            \{ txFinalFee } -> pure $ amount - txFinalFee
        , CTU.assertLossAtAddress' script amount
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
         script <- getScriptAddress
         void $ CTU.withAssertions (assertions { visitor, script }) (reward donationTxId)

main :: Effect Unit
main = CTU.interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (CTU.exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true } $
      testPlutipContracts config suite

