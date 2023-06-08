module Minting.Types.Value
      ( Value
      , ValueField
      , _value
      )
      where

import Data.BigInt (BigInt)
import Type.Proxy (Proxy (Proxy))
import Validation (FieldError)

type Value = BigInt

type ValueField :: (Type -> Type -> Type -> Type) -> Type
type ValueField f = f FieldError String Value

_value :: Proxy "value"
_value = Proxy
