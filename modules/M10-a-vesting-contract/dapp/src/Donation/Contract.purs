module Donation.Contract
  ( ownWalletAddress
  , donate
  , reclaim
  ) where

import Contract.Prelude
  ( ($)
  , (<>)
  , (<$>)
  , Unit
  , Maybe(Nothing)
  , pure
  , bind
  , discard
  , liftEither
  , wrap
  )

import Contract.Address as CA
import Contract.Monad as CM
import Contract.PlutusData as CPD
import Contract.ScriptLookups as CSL
import Contract.Scripts as CS
import Contract.Transaction as CT
import Contract.TxConstraints as CTC
import Contract.Value as CV
import Contract.Utxos as CU
import Contract.Time as CTi
import Contract.Chain as CC
import Data.Array as DA
import Data.Lens (view)
import Donation.Script (validator)
import Donation.Types
  ( DonateParams
  , ReclaimParams
  , ContractResult
  , TransactionId 
  , VestingDatum (VestingDatum)
  )

ownWalletAddress :: String -> CM.Contract CA.Address
ownWalletAddress s = CM.liftedM ("Failed to get " <> s <> " address") $
  DA.head <$> CA.getWalletAddresses

donate :: DonateParams -> CM.Contract ContractResult
donate dp = do
  validator <- liftEither validator
  beneficiary <- CM.liftContractM "failed to get pubkey" $ CA.toPubKeyHash dp.beneficiary
  let
      value = CV.lovelaceValueOf dp.value
      vhash = CS.validatorHash validator
      datum = wrap $ CPD.toData $ VestingDatum
        { beneficiary
        , deadline: dp.deadline
        }
      constraints :: CTC.TxConstraints Unit Unit
      constraints = CTC.mustPayToScript vhash datum CTC.DatumWitness value

      lookups :: CSL.ScriptLookups CPD.PlutusData
      lookups = CSL.validator validator
  ubTx <- CM.liftedE $ CSL.mkUnbalancedTx lookups constraints
  bsTx <- CM.liftedE $ CT.balanceTx ubTx
  tx <- CT.signTransaction bsTx
  txId <- CT.submit tx
  CT.awaitTxConfirmed txId
  pure { txId
       , txFinalFee: CT.getTxFinalFee tx
       }

reclaim :: ReclaimParams -> CM.Contract ContractResult
reclaim p = do
  validator <- liftEither validator
  let scriptAddress = CA.scriptHashAddress
        (CS.validatorHash validator)
        Nothing
  utxos <- CU.utxosAt scriptAddress
  utxo <- CM.liftContractM "could not find utxo at script address" $
    DA.head $ CT.lookupTxHash p.donationTxId utxos
  pk <- CM.liftContractM "could not generate PubKeyHash for beneficiary" $
    CA.toPubKeyHash p.beneficiary
  now <- CC.currentTime
  let
      txInput = view CT._input utxo
      ppkh = CA.PaymentPubKeyHash pk
      constraints :: CTC.TxConstraints Unit Unit
      constraints =    CTC.mustSpendScriptOutput txInput CPD.unitRedeemer
                    <> CTC.mustBeSignedBy ppkh
                    <> CTC.mustValidateIn (CTi.from now)

      lookups :: CSL.ScriptLookups CPD.PlutusData
      lookups =    CSL.validator validator
                <> CSL.unspentOutputs utxos
  ubTx <- CM.liftedE $ CSL.mkUnbalancedTx lookups constraints
  bsTx <- CM.liftedE $ CT.balanceTx ubTx
  tx <- CT.signTransaction bsTx
  txId <- CT.submit tx
  CT.awaitTxConfirmed txId
  pure { txId
       , txFinalFee: CT.getTxFinalFee tx
       }
