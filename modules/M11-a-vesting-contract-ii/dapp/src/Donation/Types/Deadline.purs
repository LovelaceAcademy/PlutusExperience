module Donation.Types.Deadline
      ( Deadline
      , DeadlineField
      , _deadline
      ) where

import Contract.Time as CTi
import Type.Proxy (Proxy (Proxy))
import Validation (FieldError)

type Deadline = CTi.POSIXTime

type DeadlineField :: (Type -> Type -> Type -> Type) -> Type
type DeadlineField f = f FieldError String Deadline

_deadline :: Proxy "deadline"
_deadline = Proxy
