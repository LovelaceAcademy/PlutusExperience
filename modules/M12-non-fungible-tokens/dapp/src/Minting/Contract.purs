module Minting.Contract
  ( nowDeadline
  , ownWalletAddress
  , ownBeneficiary
  , mint
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
import Contract.Time as CTi
import Contract.Chain as CC
import Contract.PlutusData as CPD
import Contract.Numeric.BigNum as CNBN
import Data.Array as DA
import Minting.Script (policy)
import Minting.Types as MT

data VestingDatum = VestingDatum
    { beneficiary :: CA.PubKeyHash
    , deadline    :: CTi.POSIXTime
    }

instance CPD.ToData VestingDatum where
  toData (VestingDatum { beneficiary, deadline }) = CPD.Constr CNBN.zero
    [ CPD.toData beneficiary
    , CPD.toData deadline
    ]

nowDeadline :: CM.Contract MT.Deadline
nowDeadline = CC.currentTime

ownWalletAddress :: String -> CM.Contract CA.Address
ownWalletAddress s = CM.liftedM ("Failed to get " <> s <> " address") $
  DA.head <$> CA.getWalletAddresses

ownBeneficiary :: CM.Contract MT.Beneficiary
ownBeneficiary = do
  ppkh <- CM.liftedM ("Failed to get beneficiary payment pub key hash")
    $ DA.head <$> CA.ownPaymentPubKeysHashes
  pure $ wrap ppkh

mint :: MT.Mint -> CM.Contract MT.ContractResult
donate dp = do
  validator <- liftEither policy
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
