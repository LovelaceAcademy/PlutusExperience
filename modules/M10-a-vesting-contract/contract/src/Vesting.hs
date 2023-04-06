{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Vesting (validator) where

import PlutusTx.Prelude
  ( ($)
  , (&&)
  , Bool
  , traceIfFalse
  )
import PlutusTx
  ( compile
  , unstableMakeIsData
  )
import Plutus.V1.Ledger.Interval (contains)
import Plutus.V2.Ledger.Api
  ( Validator
  , ScriptContext
  , PubKeyHash
  , POSIXTime
  , TxInfo
  , mkValidatorScript
  , scriptContextTxInfo
  , from
  , txInfoValidRange
  )
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.Script.Utils.Typed (IsScriptContext (mkUntypedValidator))

data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE validator_ #-}
validator_ :: VestingDatum -> () -> ScriptContext -> Bool
validator_ dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                        traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiary dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrap ||])
  where
    wrap = mkUntypedValidator validator_
