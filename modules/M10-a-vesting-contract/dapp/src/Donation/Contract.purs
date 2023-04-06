module Donation.Contract
  ( ContractResult
  , donate
  ) where

import Contract.Prelude
  ( ($)
  , Unit
  , pure
  , bind
  , discard
  , liftEither
  )

import Contract.Monad as CM
import Contract.PlutusData as CPD
import Contract.ScriptLookups as CSL
import Contract.Scripts as CS
import Contract.Transaction as CT
import Contract.TxConstraints as CTC
import Contract.Value as CV
import Data.BigInt as DBI
import Donation.Script (validator)

type ContractResult =
  { txId :: CT.TransactionHash
  , txFinalFee :: DBI.BigInt
  }
type Amount = DBI.BigInt

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
