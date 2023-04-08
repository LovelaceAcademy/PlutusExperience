module Validation
  ( FieldError
  , strIsBigInt
  , ed25519
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
  )

import Contract.Address as CA
import Ctl.Internal.Serialization.Hash as CISH
import Data.BigInt as DBI
import Data.Lens (preview)
import Formless as F

data FieldError =
    EmptyError
  | NotBigInt String
  | NotEd25519 String

instance Show FieldError where
  show EmptyError = "This field is required"
  show (NotBigInt _) = "This field os not in BigInt format"
  show (NotEd25519 _) = "This field is not an address verification key"

showError :: forall e o. Show e => F.FormFieldResult e o -> Maybe String
showError = (<$>) show <<< preview F._Error

strIsBigInt  :: forall form m. Monad m => F.Validation form m FieldError String DBI.BigInt
strIsBigInt = F.hoistFnE_ \str -> maybe (Left $ NotBigInt str) Right (DBI.fromString str)

ed25519 :: forall form m. Monad m => F.Validation form m FieldError CA.Bech32String CA.Ed25519KeyHash
ed25519 = F.hoistFnE_ $ \str -> maybe (Left $ NotEd25519 str) Right (CISH.ed25519KeyHashFromBech32 str)
