module Minting.Script
  ( PolicyParams(..)
  , policy
  )
  where

import Contract.Prelude
  ( (>>=)
  , ($)
  , (<>)
  , (<$>)
  , Maybe
  , show
  )
import Contract.TextEnvelope (plutusScriptV2FromEnvelope, decodeTextEnvelope)
import Contract.Scripts (PlutusScript, ApplyArgsError, MintingPolicy (PlutusMintingPolicy), applyArgs)
import Contract.Value (TokenName)
import Contract.PlutusData (class ToData, PlutusData(Constr), toData)
import Contract.Numeric.BigNum (zero)
import Contract.Transaction (TransactionInput)
import Data.Either (Either, note)
import Data.Bifunctor (lmap)
import Effect.Exception (Error, error)
import Scripts (scriptV2)

data PolicyParams = PolicyParams TokenName TransactionInput

instance ToData PolicyParams where
  toData (PolicyParams tn txOut) = Constr zero
    [ toData tn
    , toData txOut
    ]

parseError :: Error
parseError = error "Failed to parse script"

applyError :: ApplyArgsError -> Error
applyError err = error $ "Failed to apply arguments to the script: " <> show err

parseScript' :: Maybe PlutusScript
parseScript' = decodeTextEnvelope scriptV2 >>= plutusScriptV2FromEnvelope

parseScript :: Either Error PlutusScript
parseScript = note parseError parseScript'

applyScript :: PolicyParams -> PlutusScript -> Either Error PlutusScript
applyScript pp ps = applyError `lmap` (applyArgs ps [toData pp])

policy :: PolicyParams -> Either Error MintingPolicy
policy pp = PlutusMintingPolicy <$> (parseScript >>= applyScript pp)
