module Wallet.Api (Api, Name, enableWallet) where

import Effect (Effect)
import Promise (Promise)
import Web.HTML (Window)

type Name = String

foreign import data Api :: Type
foreign import enableWallet :: Window -> Name -> Effect (Promise Api)
