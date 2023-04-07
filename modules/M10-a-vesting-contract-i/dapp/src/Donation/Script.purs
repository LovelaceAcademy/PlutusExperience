module Donation.Script (validator) where

import Contract.Prelude
  ( (<$>)
  , (>>=)
  , Maybe
  , wrap
  )
import Contract.TextEnvelope (plutusScriptV2FromEnvelope, decodeTextEnvelope)
import Contract.Scripts (Validator, PlutusScript)
import Data.Either (Either, note)
import Effect.Exception (Error, error)
import Scripts (scriptV2)

parseError :: Error
parseError = error "Failed to parse script"

parseScript' :: Maybe PlutusScript
parseScript' = decodeTextEnvelope scriptV2 >>= plutusScriptV2FromEnvelope

parseScript :: Either Error PlutusScript
parseScript = note parseError parseScript'

validator :: Either Error Validator
validator = wrap <$> parseScript
