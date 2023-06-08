{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Minting (policy) where

import Plutus.Script.Utils.Typed (IsScriptContext (mkUntypedValidator))
import Plutus.V1.Ledger.Value (flattenValue)
import Plutus.V2.Ledger.Api
  ( ScriptContext,
    TokenName,
    TxOutRef,
    Validator (Validator),
    mkValidatorScript,
    scriptContextTxInfo,
    txInfoMint,
    txOutRefId,
    txOutRefIdx,
    unsafeFromBuiltinData,
    fromCompiledCode
  )
import Plutus.V2.Ledger.Contexts (spendsOutput)
import PlutusTx
  ( compile,
    unstableMakeIsData,
  )
import PlutusTx.Prelude
  ( Bool (False),
    traceIfFalse,
    (&&),
    (==),
    (>),
    ($),
    (.)
  )

data PolicyParams = PolicyParams TokenName TxOutRef

unstableMakeIsData ''PolicyParams

{-# INLINEABLE policy_ #-}
policy_ :: PolicyParams -> () -> () -> ScriptContext -> Bool
policy_ (PolicyParams tn txOut) () () ctx =
  traceIfFalse "Minting UTxO is not being consumed" spendsMintingUTxO
    && traceIfFalse "Minting an invalid token amount" mintsExpectedAmount
  where
    ts = 1 -- NFT supply must be one
    info = scriptContextTxInfo ctx
    -- because utxos refs are unique, when at least one output
    -- matches one input we assume that the spent event is also unique
    spendsMintingUTxO = spendsOutput info (txOutRefId txOut) (txOutRefIdx txOut)
    mintsExpectedAmount = case flattenValue (txInfoMint info) of
      [(_, tn', v)] | tn' == tn && v > 0 -> v == ts -- mint supply
      _ -> False -- otherwise

policy :: Validator
policy = Validator $ fromCompiledCode $$(compile [||wrap||])
  where
    wrap = policy_ . unsafeFromBuiltinData
