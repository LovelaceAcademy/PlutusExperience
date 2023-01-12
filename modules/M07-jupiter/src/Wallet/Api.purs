module Wallet.Api (Api, Cbor, Name, enableWallet, getBalance) where

import Effect (Effect)
import Promise (Promise)
import Web.HTML (Window)

type Name = String
type Cbor = String

foreign import data Api :: Type
foreign import enableWallet :: Window -> Name -> Effect (Promise Api)
foreign import getBalance :: Api -> Effect (Promise Cbor)
