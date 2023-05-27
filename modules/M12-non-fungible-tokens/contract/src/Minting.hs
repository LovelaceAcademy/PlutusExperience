{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Minting (validator) where

import Plutus.Script.Utils.Typed (IsScriptContext (mkUntypedValidator))
import Plutus.V1.Ledger.Interval (contains)
import Plutus.V2.Ledger.Api
  ( POSIXTime,
    PubKeyHash,
    ScriptContext,
    TxInfo,
    Validator,
    from,
    mkValidatorScript,
    scriptContextTxInfo,
    txInfoValidRange,
  )
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx
  ( compile,
    unstableMakeIsData,
  )
import PlutusTx.Prelude
  ( Bool,
    traceIfFalse,
    ($),
    (&&),
  )

data VestingDatum = VestingDatum
  { beneficiary :: PubKeyHash,
    deadline :: POSIXTime
  }

unstableMakeIsData ''VestingDatum

{-# INLINEABLE validator_ #-}
validator_ :: VestingDatum -> () -> ScriptContext -> Bool
validator_ dat () ctx =
  traceIfFalse "beneficiary's signature missing" signedByBeneficiary
    && traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiary dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info

validator :: Validator
validator = mkValidatorScript $$(compile [||wrap||])
  where
    wrap = mkUntypedValidator validator_
