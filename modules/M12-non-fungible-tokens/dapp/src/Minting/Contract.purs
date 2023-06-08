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
import Minting.Script (PolicyParams(PolicyParams), policy)
import Minting.Types as MT

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

mint :: CM.Contract MT.ContractResult
mint = do
  policy' <- liftEither $ policy (PolicyParams ?tn ?txOut)
  let
      constraints :: CTC.TxConstraints Unit Unit
      constraints =  CTC.mustMintValue ?value
                  <> CTC.mustSpendPubKeyOutput ?txOut

      lookups :: CSL.ScriptLookups CPD.PlutusData
      lookups =  CSL.mintingPolicy (CS.PlutusMintingPolicy policy')
              <> CSL.unspentOutputs ?utxos
  ubTx <- CM.liftedE $ CSL.mkUnbalancedTx lookups constraints
  bsTx <- CM.liftedE $ CT.balanceTx ubTx
  tx <- CT.signTransaction bsTx
  txId <- CT.submit tx
  CT.awaitTxConfirmed txId
  pure { txId
       , txFinalFee: CT.getTxFinalFee tx
       }
