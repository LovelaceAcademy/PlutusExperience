module Minting.Types
  ( module Minting.Types.Beneficiary
  , module Minting.Types.Deadline
  , module Minting.Types.Value
  , ContractResult
  , TransactionId
  , Common
  , Address
  )
  where

import Contract.Address as CA
import Contract.Transaction as CT
import Minting.Types.Beneficiary
import Minting.Types.Deadline
import Minting.Types.Value

type TransactionId = CT.TransactionHash
type Address = CA.Address
type Common = ( beneficiary :: Beneficiary )
type ContractResult =
  { txId :: TransactionId
  , txFinalFee :: Value
  }
