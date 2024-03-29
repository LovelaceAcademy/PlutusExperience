module Minting.Contract
  ( nowDeadline
  , ownWalletAddress
  , ownBeneficiary
  , mint
  , mkCurrencySymbol
  , mkTokenName
  , pickTxOut
  ) where

import Contract.Prelude
  ( ($)
  , (<>)
  , (<$>)
  , (=<<)
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
import Contract.Utxos as CU
import Contract.Prim.ByteArray as CPBA
import Contract.Wallet as CW
import Data.Array as DA
import Data.Map as DM
import Data.BigInt as DBI
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

mkTokenName :: String -> CM.Contract CV.TokenName
mkTokenName n = CM.liftContractM "Failed to make token name"
  $ CV.mkTokenName =<< CPBA.byteArrayFromAscii n

mkCurrencySymbol :: CS.MintingPolicy -> CM.Contract CV.CurrencySymbol
mkCurrencySymbol p = CM.liftContractM "Failed to get script currency symbol"
  $ CV.scriptCurrencySymbol p


pickTxOut :: CU.UtxoMap -> CM.Contract CT.TransactionInput
pickTxOut utxos = do
  { key: txOut } <- CM.liftContractM "Failed to get the first utxo"
    $ DM.findMin utxos
  pure txOut

mint :: CM.Contract MT.ContractResult
mint = do
  tn <- mkTokenName "MyOwnNFT"
  utxos <- CM.liftedM "Failed to get wallet utxos" CW.getWalletUtxos
  txOut <- pickTxOut utxos
  policy' <- liftEither $ policy (PolicyParams tn txOut)
  cur <- mkCurrencySymbol policy'
  let
      value = CV.singleton cur tn (DBI.fromInt 1)
      constraints :: CTC.TxConstraints Unit Unit
      constraints =  CTC.mustMintValue value
                  <> CTC.mustSpendPubKeyOutput txOut

      lookups :: CSL.ScriptLookups CPD.PlutusData
      lookups =  CSL.mintingPolicy policy'
              <> CSL.unspentOutputs utxos
  ubTx <- CM.liftedE $ CSL.mkUnbalancedTx lookups constraints
  bsTx <- CM.liftedE $ CT.balanceTx ubTx
  tx <- CT.signTransaction bsTx
  txId <- CT.submit tx
  CT.awaitTxConfirmed txId
  pure { txId
       , txFinalFee: CT.getTxFinalFee tx
       }
