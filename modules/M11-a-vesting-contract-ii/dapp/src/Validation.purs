module Validation
  ( FieldError (..)
  , strIsBigInt
  , strIsByteArray
  , hoistFnM_
  , showError
  )
  where

import Contract.Prelude
  ( class Monad
  , class Show
  , show
  , ($)
  , (<$>)
  , (<<<)
  , Either(Left, Right)
  , Maybe
  , maybe
  , note
  )

import Contract.Address as CA
import Contract.Prim.ByteArray as CPBA
import Ctl.Internal.Serialization.Hash as CISH
import Data.BigInt as DBI
import Data.Lens (preview)
import Formless as F

data FieldError =
    EmptyError
  | NotBigInt String
  | NotByteArray String
  | NotEd25519 String
  | Other String

instance Show FieldError where
  show EmptyError = "This field is required"
  show (NotBigInt _) = "This field os not in BigInt format"
  show (NotByteArray _) = "This field os not in ByteArray format"
  show (NotEd25519 _) = "This field is not an address verification key"
  show (Other s) = s

showError :: forall e o. Show e => F.FormFieldResult e o -> Maybe String
showError = (<$>) show <<< preview F._Error

strIsBigInt  :: forall form m. Monad m => F.Validation form m FieldError String DBI.BigInt
strIsBigInt = F.hoistFnE_ \str -> maybe (Left $ NotBigInt str) Right (DBI.fromString str)


strIsByteArray  :: forall form m. Monad m => F.Validation form m FieldError String CA.ByteArray 
strIsByteArray = F.hoistFnE_ $ \str -> maybe (Left $ NotByteArray str) Right (CPBA.byteArrayFromAscii str)

hoistFnM_ :: forall form m e i o. Monad m => e -> (i -> Maybe o) -> F.Validation form m e i o
hoistFnM_ err cb = F.hoistFnE_ (\i -> note err (cb i))
