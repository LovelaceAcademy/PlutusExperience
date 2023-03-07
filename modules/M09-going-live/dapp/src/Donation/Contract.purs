module Donation.Contract 
  ( ContractResult
  , donation
  , reward
  ) where

import Contract.Prelude
  ( ($)
  , (<>)
  , Unit
  , liftEither
  , pure
  , bind
  , discard
  , wrap
  )
import Contract.Address as CA
import Contract.Monad 
  ( Contract
  , liftedE
  , liftContractM
  )
import Contract.Transaction as CT
import Contract.ScriptLookups as CSL
import Contract.TxConstraints as CTC
import Contract.PlutusData as CPD
import Contract.Value as CV
import Contract.Scripts (validatorHash)
import Contract.Utxos as CU
import Data.BigInt as DBI
import Data.Array (head)
import Data.Lens (view)
import Data.Maybe (Maybe(Nothing))
import Donation.Script (validator)

type ContractResult =
  { txId :: CT.TransactionHash
  , txFinalFee :: DBI.BigInt
  }

type TransactionId = CT.TransactionHash

newtype Password = Password DBI.BigInt

instance CPD.ToData Password where
  toData (Password n) = CPD.Constr (DBI.fromInt 0) [CPD.toData n]

donation :: Contract () ContractResult
donation = do
  validator <- liftEither validator
  let
      value = CV.lovelaceValueOf $ DBI.fromInt 10_000_000
      vhash = validatorHash validator

      constraints :: CTC.TxConstraints Unit Unit
      constraints = CTC.mustPayToScript vhash CPD.unitDatum CTC.DatumWitness value

      lookups :: CSL.ScriptLookups CPD.PlutusData
      lookups = CSL.validator validator
  ubTx <- liftedE $ CSL.mkUnbalancedTx lookups constraints
  bsTx <- liftedE $ CT.balanceTx ubTx
  tx <- CT.signTransaction bsTx
  txId <- CT.submit tx
  CT.awaitTxConfirmed txId
  pure { txId
       , txFinalFee: CT.getTxFinalFee tx
       }

reward :: forall r. TransactionId -> Contract r ContractResult
reward donationTxId = do
  validator <- liftEither validator
  let scriptAddress = CA.scriptHashAddress (validatorHash validator) Nothing
  utxos <- CU.utxosAt scriptAddress
  utxo <- liftContractM "could not find utxo at script address" $
    head $ CT.lookupTxHash donationTxId utxos
  let
      txInput = view CT._input utxo
      redeemer = wrap $ CPD.toData $ Password $ DBI.fromInt 42

      constraints :: CTC.TxConstraints Unit Unit
      constraints = CTC.mustSpendScriptOutput txInput redeemer

      lookups :: CSL.ScriptLookups CPD.PlutusData
      lookups =    CSL.validator validator
                <> CSL.unspentOutputs utxos
  ubTx <- liftedE $ CSL.mkUnbalancedTx lookups constraints
  bsTx <- liftedE $ CT.balanceTx ubTx
  tx <- CT.signTransaction bsTx
  txId <- CT.submit tx
  CT.awaitTxConfirmed txId
  pure { txId
       , txFinalFee: CT.getTxFinalFee tx
       }

