module Donation.Contract
  ( nowDeadline
  , ownWalletAddress
  , ownBeneficiary
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
  , unwrap
  )

import Contract.Address as CA
import Contract.Monad as CM
import Contract.ScriptLookups as CSL
import Contract.Scripts as CS
import Contract.Transaction as CT
import Contract.TxConstraints as CTC
import Contract.Value as CV
import Contract.Utxos as CU
import Contract.Time as CTi
import Contract.Chain as CC
import Contract.PlutusData as CPD
import Contract.Numeric.BigNum as CNBN
import Data.Array as DA
import Data.Lens (view)
import Donation.Script (validator)
import Donation.Types as DT

data VestingDatum = VestingDatum
    { beneficiary :: CA.PubKeyHash
    , deadline    :: CTi.POSIXTime
    }

instance CPD.ToData VestingDatum where
  toData (VestingDatum { beneficiary, deadline }) = CPD.Constr CNBN.zero
    [ CPD.toData beneficiary
    , CPD.toData deadline
    ]

nowDeadline :: CM.Contract DT.Deadline
nowDeadline = CC.currentTime

ownWalletAddress :: String -> CM.Contract CA.Address
ownWalletAddress s = CM.liftedM ("Failed to get " <> s <> " address") $
  DA.head <$> CA.getWalletAddresses

ownBeneficiary :: CM.Contract DT.Beneficiary
ownBeneficiary = do
  ppkh <- CM.liftedM ("Failed to get beneficiary payment pub key hash")
    $ DA.head <$> CA.ownPaymentPubKeysHashes
  pure $ wrap ppkh

donate :: DT.Donate -> CM.Contract DT.ContractResult
donate dp = do
  validator <- liftEither validator
  let
      pkh   = unwrap $ unwrap dp.beneficiary
      value = CV.lovelaceValueOf dp.value
      vhash = CS.validatorHash validator
      datum = wrap $ CPD.toData $ VestingDatum
        { beneficiary: pkh
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

reclaim :: DT.Reclaim -> CM.Contract DT.ContractResult
reclaim p = do
  validator <- liftEither validator
  let scriptAddress = CA.scriptHashAddress
        (CS.validatorHash validator)
        Nothing
  utxos <- CU.utxosAt scriptAddress
  utxo <- CM.liftContractM "could not find utxo at script address" $
    DA.head $ CT.lookupTxHash p.donationTxId utxos
  now <- CC.currentTime
  let
      txInput = view CT._input utxo
      constraints :: CTC.TxConstraints Unit Unit
      constraints =    CTC.mustSpendScriptOutput txInput CPD.unitRedeemer
                    <> CTC.mustBeSignedBy (unwrap p.beneficiary)
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
