module Donation.Script (parseScript, validator) where

import Contract.Prelude

import Cardano.TextEnvelope (TextEnvelopeType (PlutusScriptV2), textEnvelopeBytes, printTextEnvelopeDecodeError)
import Contract.Scripts (Validator, PlutusScript)
import Contract.Transaction (plutusV2Script)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect.Exception (Error, error)

foreign import script :: String

parseScript :: Either Error PlutusScript
parseScript = plutusV2Script <$> (lmap (error <<< printTextEnvelopeDecodeError) $ textEnvelopeBytes script PlutusScriptV2)

validator :: Either Error Validator
validator = wrap <$> parseScript

