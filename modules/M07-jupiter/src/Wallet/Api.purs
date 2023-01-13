module Wallet.Api
  ( Api
  , Cbor
  , Hex
  , Json
  , Value
  , Name
  , Utxo
  , TxOut
  , Coin
  , enableWallet
  , getUtxos
  , fromHexToUtxo
  , amount
  , output
  , coin
  ) where

import Effect (Effect)
import Promise (Promise)
import Web.HTML (Window)

type Name = String
type Cbor = String
type Json = String
type Hex = String
type Coin = Number

foreign import data Api :: Type
foreign import data Value :: Type
foreign import data Utxo :: Type
foreign import data TxOut :: Type

foreign import enableWallet :: Window -> Name -> Effect (Promise Api)
foreign import getUtxos :: Api -> Effect (Promise (Array Cbor))
foreign import fromHexToUtxo :: Hex -> Effect Utxo
foreign import amount :: TxOut -> Value
foreign import output :: Utxo -> TxOut
foreign import coin :: Value -> Effect Coin
