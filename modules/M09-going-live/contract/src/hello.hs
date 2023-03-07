{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main (main) where

import Prelude (IO)
import PlutusTx.Prelude (($), (==), Maybe (Nothing), Bool (True, False), Integer, error)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2)
import Cardano.Api.SerialiseTextEnvelope (textEnvelopeToJSON)
import qualified Codec.Serialise as CS
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Short as DBS
import PlutusTx (compile, unstableMakeIsData)
import PlutusTx.Builtins.Internal (BuiltinData)
import Plutus.V2.Ledger.Api (Validator, ScriptContext, mkValidatorScript)
import Plutus.Script.Utils.Typed (IsScriptContext (mkUntypedValidator))

newtype Password = Password Integer
unstableMakeIsData ''Password

validator :: BuiltinData -> Password -> ScriptContext -> Bool
validator _ (Password n) _ | n == 42 = True 
validator _ _ _ = error () 

validator' :: Validator
validator' = mkValidatorScript $$(compile [|| wrap ||])
  where
    wrap = mkUntypedValidator validator

serialise :: Validator -> PlutusScript PlutusScriptV2
serialise val = PlutusScriptSerialised $ DBS.toShort $ DBL.toStrict $ CS.serialise $ val

main :: IO ()
main = DBL.putStr $ textEnvelopeToJSON Nothing $ serialise validator'
