{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main (main) where

import Prelude (IO)
import PlutusTx.Prelude (($), (==), Maybe (Nothing), Bool (True, False), Integer)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2)
import Cardano.Api.SerialiseTextEnvelope (textEnvelopeToJSON)
import qualified Codec.Serialise as CS
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Short as DBS
import PlutusTx (compile, unstableMakeIsData)
import Plutus.V2.Ledger.Api (Validator, ScriptContext, mkValidatorScript)
import Plutus.Script.Utils.Typed (IsScriptContext (mkUntypedValidator))

newtype MyDatum = MyDatum Integer
unstableMakeIsData ''MyDatum
newtype MyRedeemer = MyRedeemer Integer
unstableMakeIsData ''MyRedeemer

validator :: MyDatum -> MyRedeemer -> ScriptContext -> Bool
validator _ (MyRedeemer n) _ | n == 42 = True 
validator _ _ _ = False 

validator' :: Validator
validator' = mkValidatorScript $$(compile [|| wrap ||])
  where
    wrap = mkUntypedValidator validator

serialise :: Validator -> PlutusScript PlutusScriptV2
serialise val = PlutusScriptSerialised $ DBS.toShort $ DBL.toStrict $ CS.serialise $ val

main :: IO ()
main = DBL.putStr $ textEnvelopeToJSON Nothing $ serialise validator'
