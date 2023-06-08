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
  , wrap
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
import Minting
  ( ContractResult
  , policy
  , mint
  )
import Minting.Types
  ( Address
  )

type Labeled = CTA.Labeled
type Contract = CM.Contract
type Params = ( script :: Labeled Address )
type MintParams = { visitor :: Labeled Address | Params  }
type RewardParams = { beneficiary :: Labeled Address | Params }

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

withPaymentPubKeyHash :: forall a. Labeled Address -> (CA.PaymentPubKeyHash -> Contract a) -> Contract a
withPaymentPubKeyHash addr run = do
  pkh <- CM.liftContractM "failed to get address pub key hash" $
    CA.toPubKeyHash (CTA.unlabel addr)
  run $ CA.PaymentPubKeyHash pkh

getOwnWalletLabeledAddress :: String -> Contract (Labeled Address)
getOwnWalletLabeledAddress s = do
       addr <- CM.liftedM ("Failed to get " <> s <> " address") $
         DA.head <$> CA.getWalletAddresses
       pure $ CTA.label addr s

getScriptAddress :: Contract (Labeled Address)
getScriptAddress = do
        policy' <- CM.liftContractE' "Failed to parse policy" $
         CS.validatorHash <$> policy
        nId <- CA.getNetworkId
        valAddr <- CM.liftContractM "Failed to get policy address" $
          CA.validatorHashEnterpriseAddress nId policy'
        pure $ CTA.label valAddr "script"

suite :: TestPlanM ContractTest Unit
suite = do
  test "visitor mints the NFT" do
    let
      distribution :: InitialUTxOs
      distribution =
        [ DBI.fromInt 5_000_000
        , DBI.fromInt 2_000_000_000
        ]
      checks :: MintParams -> Array (CTA.ContractCheck ContractResult)
      checks { visitor, script } = let amount = DBI.fromInt 10_000_000 in
        [ CTA.checkLossAtAddress visitor
          \r -> do
             { txFinalFee } <- CM.liftContractM "contract did not provided value" r
             pure $ amount + txFinalFee
        , CTA.checkGainAtAddress' script amount
        ]
    withWallets distribution \kw -> withKeyWallet kw do
       visitor <- getOwnWalletLabeledAddress "visitor"
       script <- getScriptAddress
       void $ CTA.runChecks
        ( checks { visitor, script } )
        ( lift $ withPaymentPubKeyHash visitor \_ -> mint )

main :: Effect Unit
main = CTU.interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (CTU.exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true } $
      testPlutipContracts config suite

