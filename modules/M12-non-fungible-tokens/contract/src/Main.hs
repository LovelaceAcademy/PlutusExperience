{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Cardano.Api.SerialiseTextEnvelope (textEnvelopeToJSON)
import Cardano.Api.Shelley
  ( PlutusScript (PlutusScriptSerialised),
    PlutusScriptV2,
  )
import qualified Codec.Serialise as CS
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Short as DBS
import Minting (validator)
import Plutus.V2.Ledger.Api (Validator)
import PlutusTx.Prelude
  ( Maybe (Nothing),
    ($),
    (.),
  )
import Prelude (IO)

serialise :: Validator -> PlutusScript PlutusScriptV2
serialise =
  PlutusScriptSerialised
    . DBS.toShort
    . DBL.toStrict
    . CS.serialise

main :: IO ()
main =
  DBL.putStr $
    textEnvelopeToJSON Nothing $
      serialise validator
