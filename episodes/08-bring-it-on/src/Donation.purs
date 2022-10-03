module Donation (component) where

import Contract.Prelude

import Control.Monad.State (get, put)
import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (liftMaybe, throwError)
import Contract.Address (scriptHashAddress)
import Contract.Config (ConfigParams)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM, liftedE, runContract)
import Contract.PlutusData (PlutusData, unitDatum, unitRedeemer)
import Contract.Transaction (submit, balanceAndSignTx, lookupTxHash)
import Contract.TxConstraints as CT
import Contract.ScriptLookups as SL
import Contract.Value as V
import Contract.Wallet (WalletSpec (UseKeys), PrivatePaymentKeySource (PrivatePaymentKeyValue), PrivatePaymentKey (PrivatePaymentKey))
import Contract.Utxos (utxosAt)
import Data.BigInt as BI
import Data.Map as Map
import Data.Array as Array
import Data.Lens (view)
import Donation.Script (toValidator)
import Effect.Exception (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Plutus.Types.TransactionUnspentOutput (_input)
import Scripts (validatorHash)
import Serialization (privateKeyFromBytes)
import Types.RawBytes (hexToRawBytes)
import Types.Transaction (TransactionHash)

data Action = SetDonatorKey String | SetVisitorKey String

buildBalanceSignAndSubmitTx lookups constraints = do
  ubTx <- liftedE $ SL.mkUnbalancedTx lookups constraints
  bsTx <- liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId
  pure txId

give :: forall r. Int -> Contract r TransactionHash
give amount = do
  validator <- liftEither $ toValidator
  let 
      value = V.lovelaceValueOf $ BI.fromInt amount
      vhash = validatorHash validator
  
      constraints :: CT.TxConstraints Unit Unit
      constraints = CT.mustPayToScript vhash unitDatum CT.DatumWitness value

      lookups :: SL.ScriptLookups PlutusData
      lookups = SL.validator validator
  buildBalanceSignAndSubmitTx lookups constraints

grab :: forall r. TransactionHash -> Contract r Unit
grab txId = do
  validator <- liftEither $ toValidator
  let scriptAddress = scriptHashAddress $ validatorHash validator
  utxos <- (fromMaybe Map.empty) <$> (utxosAt scriptAddress)
  utxo <- liftMaybe (error "Could not find any locked value") $ Array.head (lookupTxHash txId utxos)
  let
    txInput = view _input utxo

    constraints :: CT.TxConstraints Unit Unit
    constraints = CT.mustSpendScriptOutput txInput unitRedeemer

    lookups :: SL.ScriptLookups PlutusData
    lookups =    SL.validator validator
              <> SL.unspentOutputs utxos
  void $ buildBalanceSignAndSubmitTx lookups constraints

mkWalletSpec :: String -> Effect (Maybe WalletSpec)
mkWalletSpec key = do
        pk <- liftMaybe (error "Failed to parse private key") $ privateKeyFromBytes =<< hexToRawBytes key
        let ws = UseKeys (PrivatePaymentKeyValue $ PrivatePaymentKey pk) Nothing
        pure $ Just ws

component :: forall q i o r. ConfigParams r -> H.Component q i o Aff
component cfg = H.mkComponent
  { initialState: const Nothing
  , eval: H.mkEval $ H.defaultEval
    { handleAction = case _ of
      (SetDonatorKey key) -> do
        ws <- liftEffect $ mkWalletSpec key
        let cfg' = cfg { walletSpec = ws }
        txId <- lift $ runContract cfg' $ give 10
        put $ Just txId
      (SetVisitorKey key) -> do
        ws <- liftEffect $ mkWalletSpec key
        let cfg' = cfg { walletSpec = ws }
        state <- get
        case state of
             Just txId -> lift $ runContract cfg' $ grab txId
             Nothing -> throwError $ error "Set donator private key first"
    }
  , render: \_ -> HH.div_ [
        HH.input
          [ HP.placeholder "donator secret key"
          , HE.onValueChange SetDonatorKey
          ]
      , HH.input
          [ HP.placeholder "visitor secret key"
          , HE.onValueChange SetVisitorKey
          ]
    ]

  }
