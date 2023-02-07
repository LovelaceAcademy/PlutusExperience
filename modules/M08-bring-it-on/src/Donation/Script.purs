module Donation.Script (parseScript, toValidator) where

import Contract.Prelude

import Contract.TextEnvelope (plutusScriptV2FromEnvelope, decodeTextEnvelope)
import Contract.Scripts (Validator, PlutusScript)
import Contract.Transaction (plutusV2Script)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Effect.Exception (Error, error)

foreign import script :: String

parseError :: Error
parseError = error "Failed to parse script"

parseScript' :: Maybe PlutusScript
parseScript' = decodeTextEnvelope script >>= plutusScriptV2FromEnvelope

parseScript :: Either Error PlutusScript
parseScript = note parseError  parseScript'

toValidator :: Either Error Validator
toValidator = wrap <$> parseScript
