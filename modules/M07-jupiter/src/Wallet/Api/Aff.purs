module Wallet.Api.Aff (enableWallet, getUtxos) where

import Prelude (($), (>>=), bind)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Promise.Aff (toAff)
import Wallet.Api as WA
import Web.HTML (Window)

enableWallet :: Window -> WA.Name -> Aff WA.Api
enableWallet window name = liftEffect (WA.enableWallet window name) >>= toAff

getUtxos :: WA.Api -> Aff (Array WA.Utxo)
getUtxos api = do
  cbors <- getUtxos_ api
  liftEffect $ traverse WA.fromHexToUtxo cbors

getUtxos_ :: WA.Api -> Aff (Array WA.Cbor)
getUtxos_ api = liftEffect (WA.getUtxos api)  >>= toAff 
