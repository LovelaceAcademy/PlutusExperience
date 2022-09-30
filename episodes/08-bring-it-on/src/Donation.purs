module Donation (component) where

import Contract.Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (liftMaybe)
import Contract.Config (ConfigParams)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM, liftedE, launchAff_, runContract, askConfig)
import Contract.PlutusData (PlutusData, unitDatum)
import Contract.Transaction (submit, balanceAndSignTx)
import Contract.TxConstraints as CT
import Contract.ScriptLookups as SL
import Contract.Value as V
import Contract.Wallet (WalletSpec (UseKeys), PrivatePaymentKeySource (PrivatePaymentKeyValue), PrivatePaymentKey (PrivatePaymentKey))
import Data.BigInt as BI
import Donation.Script (toValidator)
import Effect.Aff (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Scripts (validatorHash)
import Serialization (privateKeyFromBytes)
import Types.RawBytes (hexToRawBytes)

data Action = SetWalletKey String

contract :: forall r. Int -> Contract r Unit
contract amount = do
  validator <- liftEither $ toValidator
  let 
      value = V.lovelaceValueOf $ BI.fromInt amount
      vhash = validatorHash validator
  
      constraints :: CT.TxConstraints Unit Unit
      constraints = CT.mustPayToScript vhash unitDatum CT.DatumWitness value

      lookups :: SL.ScriptLookups PlutusData
      lookups = SL.validator validator
  ubTx <- liftedE $ SL.mkUnbalancedTx lookups constraints
  bsTx <- liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
  txId <- submit bsTx
  logInfo' $ "Tx ID: " <> show txId

component :: forall q i o r. ConfigParams r -> H.Component q i o Aff
component cfg = H.mkComponent
  { initialState: const ""
  , eval: H.mkEval $ H.defaultEval
    { handleAction = case _ of
      (SetWalletKey key) -> do
        pk <- liftMaybe (error "Failed to parse private key") $ privateKeyFromBytes =<< hexToRawBytes key
        let ws = UseKeys (PrivatePaymentKeyValue $ PrivatePaymentKey pk) Nothing
            cfg' = cfg { walletSpec = Just ws }
        lift $ runContract cfg' $ contract 10
        H.modify_ $ const key
    }
  , render: \s -> HH.input
      [ HP.placeholder "wallet secret key"
      , HE.onValueChange SetWalletKey
      , HP.value s
      ]

  }
