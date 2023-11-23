module Minting.Types.Beneficiary
      ( Beneficiary (Beneficiary)
      , BeneficiaryField
      , _beneficiary
      , beneficiary_label
      , beneficiary_help
      , beneficiary_placeholder 
      , beneficiary_error
      )
      where

import Contract.Prelude
      ( class Generic
      , class Newtype
      , class Show
      , (<<<)
      , (<$>)
      , unwrap
      , wrap
      )
import Contract.Address (Bech32String, PaymentPubKeyHash)
import Data.String.Read (class Read)
import Type.Proxy (Proxy (Proxy))
import Ctl.Internal.Serialization.Hash (ed25519KeyHashToBech32Unsafe, ed25519KeyHashFromBech32)
import Validation (FieldError (Other))

newtype Beneficiary = Beneficiary PaymentPubKeyHash

derive instance Generic Beneficiary _
derive instance Newtype Beneficiary _

instance Show Beneficiary where
      show =
            ed25519KeyHashToBech32Unsafe "addr_vkh"
            <<< unwrap
            <<< unwrap 
            <<< unwrap

instance Read Beneficiary where
      read s = (wrap <<< wrap <<< wrap) <$> ed25519KeyHashFromBech32 s

type BeneficiaryField :: (Type -> Type -> Type -> Type) -> Type
type BeneficiaryField f = f FieldError Bech32String Beneficiary

_beneficiary :: Proxy "beneficiary"
_beneficiary = Proxy

beneficiary_label :: String
beneficiary_label = "Address verification key hash"

beneficiary_help  :: String
beneficiary_help = "Paste the beneficiary verification key hash"

beneficiary_placeholder :: String
beneficiary_placeholder = "addr_vkh..."

beneficiary_error :: FieldError
beneficiary_error = Other "Beneficiary is not an address verification key"
