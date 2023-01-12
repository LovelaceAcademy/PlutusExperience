module Wallet.Api.Aff (enableWallet) where

import Control.Bind ((>>=))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Promise.Aff (toAff)
import Wallet.Api as WA
import Web.HTML (Window)

enableWallet :: Window -> WA.Name -> Aff WA.Api
enableWallet window name = liftEffect (WA.enableWallet window name) >>= toAff
