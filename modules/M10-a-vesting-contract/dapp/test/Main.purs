module Test.Main where

import Contract.Prelude
  ( ($)
  , (=<<)
  , (+)
  , (-)
  , (<>)
  , (<$>)
  , LogLevel (Trace)
  , Unit
  , flip
  , bind
  , pure
  , void
  , discard
  )

import Control.Monad.Trans.Class (lift)
import Contract.Address as CA
import Contract.Config (emptyHooks)
import Contract.Monad as CM
import Contract.Test
  ( ContractTest
  , InitialUTxOs
  , withWallets
  , withKeyWallet
  )
import Contract.Test.Mote
  ( TestPlanM
  , interpretWithConfig
  )
import Contract.Test.Plutip
  ( PlutipConfig
  , testPlutipContracts
  )
import Contract.Test.Utils as CTU
import Contract.Test.Assert as CTA
import Contract.Scripts as CS
import Contract.Chain as CC
import Data.Array as DA
import Data.BigInt as DBI
import Data.Maybe (Maybe (Just, Nothing))
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Seconds (Seconds))
import Data.Tuple.Nested ((/\))
import Data.UInt as DU
import Effect (Effect)
import Effect.Aff
  ( Milliseconds(Milliseconds)
  , cancelWith
  , effectCanceler
  , launchAff
  )
import Mote (test)
import Test.Spec.Runner (defaultConfig)
import Donation
  ( ContractResult
  , validator
  , donate
  , reclaim
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
  , kupoConfig:
      { port: DU.fromInt 1443
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: emptyHooks
  , clusterConfig:
      { slotLength: Seconds 0.05 }
  }

type Address = CTA.Labeled CA.Address
type DonationParams = { donator :: Address, script :: Address }
type RewardParams = { beneficiary :: Address }

getOwnWalletLabeledAddress :: String -> CM.Contract Address
getOwnWalletLabeledAddress s = do
       addr <- CM.liftedM ("Failed to get " <> s <> " address") $
         DA.head <$> CA.getWalletAddresses
       pure $ CTA.label addr s

getScriptAddress :: CM.Contract Address
getScriptAddress = do
        validator' <- CM.liftContractE' "Failed to parse validator" $
         CS.validatorHash <$> validator
        nId <- CA.getNetworkId
        valAddr <- CM.liftContractM "Failed to get validator address" $
          CA.validatorHashEnterpriseAddress nId validator'
        pure $ CTA.label valAddr "script"

suite :: TestPlanM ContractTest Unit
suite = do
  test "donator locks ADA, deadline and beneficiary in the contract" do
    let
      distribution :: InitialUTxOs
      distribution =
        [ DBI.fromInt 5_000_000
        , DBI.fromInt 2_000_000_000
        ]
      checks :: DonationParams -> Array (CTA.ContractCheck ContractResult)
      checks { donator, script } = let amount = DBI.fromInt 10_000_000 in
        [ CTA.checkLossAtAddress donator
          \r -> do
             { txFinalFee } <- CM.liftContractM "contract did not provided value" r
             pure $ amount + txFinalFee
        , CTA.checkGainAtAddress' script amount
        ]
    withWallets distribution \kw -> withKeyWallet kw do
       donator <- getOwnWalletLabeledAddress "donator"
       script <- getScriptAddress
       deadline <- CC.currentTime
       let value = DBI.fromInt 10_000_000
           beneficiary = CTA.unlabel donator
       void $ CTA.runChecks
        (checks { donator, script })
        (lift $ donate { value, beneficiary, deadline })
  test "beneficiary reclaim ADA in the contract, after the deadline" do
    let
      distribution =
           [ DBI.fromInt 20_000_000
           , DBI.fromInt 5_000_000
           ]
        /\ [ DBI.fromInt 1_000_000
           , DBI.fromInt 5_000_000
           ]
      checks :: RewardParams -> Array (CTA.ContractCheck ContractResult)
      checks { beneficiary } = let amount = DBI.fromInt 10_000_000 in
        [ CTA.checkGainAtAddress beneficiary
          \r -> do
             { txFinalFee } <- CM.liftContractM "contract did not provided any value" r
             pure $ amount - txFinalFee
        ]
    withWallets distribution \(w1 /\ w2) -> do
       beneficiary <- withKeyWallet w2 $ getOwnWalletLabeledAddress "beneficiary"
       let value = DBI.fromInt 10_000_000
       { txId: donationTxId } <- withKeyWallet w1 do
          deadline <- CC.currentTime
          donate { value
                 , beneficiary: CTA.unlabel beneficiary
                 , deadline
                 }
       withKeyWallet w2 do
          void $ CTA.runChecks
            (checks { beneficiary })
            (lift $ reclaim { beneficiary: CTA.unlabel beneficiary
                            , donationTxId
                            }
            )

main :: Effect Unit
main = CTU.interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (CTU.exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true } $
      testPlutipContracts config suite

