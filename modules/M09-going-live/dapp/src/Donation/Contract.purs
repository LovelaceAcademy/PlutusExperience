module Donation.Contract (ContractResult, contract) where

import Contract.Prelude
  ( ($)
  , Unit
  , liftEither
  , pure
  , bind
  , discard
  )
import Contract.Monad (Contract, liftedE)
import Contract.Transaction as CT
import Contract.ScriptLookups as CSL
import Contract.TxConstraints as CTC
import Contract.PlutusData as CPD
import Contract.Value as CV
import Contract.Scripts (validatorHash)
import Data.BigInt as DBI
import Donation.Script (validator)

type ContractResult =
  { txId :: CT.TransactionHash
  , txFinalFee :: DBI.BigInt
  }

contract :: Contract () ContractResult
contract = do
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
  pure {
    txId,
    txFinalFee: CT.getTxFinalFee tx
  }
