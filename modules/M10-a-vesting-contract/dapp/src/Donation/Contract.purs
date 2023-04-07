module Donation.Contract
  ( ContractResult
  , donate
  , reclaim
  ) where

import Contract.Prelude
  ( ($)
  , (<>)
  , Unit
  , Maybe(Nothing)
  , pure
  , bind
  , discard
  , liftEither
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
import Data.Array as DA
import Data.BigInt as DBI
import Data.Lens (view)
import Donation.Script (validator)

type ContractResult =
  { txId :: CT.TransactionHash
  , txFinalFee :: DBI.BigInt
  }
type Amount = DBI.BigInt
type TransactionId = CT.TransactionHash

donate :: Amount -> CM.Contract ContractResult
donate n = do
  validator <- liftEither validator
  let
      value = CV.lovelaceValueOf n
      vhash = CS.validatorHash validator
      constraints :: CTC.TxConstraints Unit Unit
      constraints = CTC.mustPayToScript vhash CPD.unitDatum CTC.DatumWitness value

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

reclaim :: TransactionId -> CM.Contract ContractResult
reclaim donationTxId = do
  validator <- liftEither validator
  let scriptAddress = CA.scriptHashAddress
        (CS.validatorHash validator)
        Nothing
  utxos <- CU.utxosAt scriptAddress
  utxo <- CM.liftContractM "could not find utxo at script address" $
    DA.head $ CT.lookupTxHash donationTxId utxos
  let
      txInput = view CT._input utxo
      constraints :: CTC.TxConstraints Unit Unit
      constraints = CTC.mustSpendScriptOutput txInput CPD.unitRedeemer

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
