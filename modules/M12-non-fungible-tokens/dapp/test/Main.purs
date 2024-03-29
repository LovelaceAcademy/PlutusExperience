module Test.Main where

import Contract.Prelude
  ( ($)
  , (=<<)
  , (<>)
  , (<$>)
  , LogLevel (Trace)
  , Unit
  , flip
  , bind
  , pure
  , void
  , liftEither
  )

import Control.Monad.Trans.Class (lift)
import Contract.Address as CA
import Contract.Config (emptyHooks)
import Contract.Monad as CM
import Contract.Value as CV
import Contract.Wallet as CW
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
import Data.Array as DA
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
import Minting
  ( ContractResult
  , PolicyParams (PolicyParams)
  , mkCurrencySymbol
  , mkTokenName
  , pickTxOut
  , policy
  , mint
  )
import Minting.Types
  ( Address
  )

type Labeled = CTA.Labeled
type Contract = CM.Contract
type MintParams = { visitor :: Labeled Address
                  , curSymbol :: CV.CurrencySymbol
                  , tokenName :: CV.TokenName
                  }

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
      checks { visitor, curSymbol, tokenName } = let amount = DBI.fromInt 1 in
        [ CTA.checkLossAtAddress visitor
          \r -> do
             { txFinalFee } <- CM.liftContractM "contract did not provided value" r
             pure txFinalFee
        , CTA.checkTokenGainAtAddress' visitor (curSymbol /\ tokenName /\ amount)
        ]
    withWallets distribution \kw -> withKeyWallet kw do
       visitor <- getOwnWalletLabeledAddress "visitor"
       tokenName <- mkTokenName "MyOwnNFT"
       utxos <- CM.liftedM "Failed to get wallet utxos" CW.getWalletUtxos
       txOut <- pickTxOut utxos
       policy' <- liftEither $ policy (PolicyParams tokenName txOut)
       curSymbol <- mkCurrencySymbol policy'
       void $ CTA.runChecks
        ( checks { visitor, curSymbol, tokenName } )
        ( lift $ withPaymentPubKeyHash visitor \_ -> mint )

main :: Effect Unit
main = CTU.interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (CTU.exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true } $
      testPlutipContracts config suite

