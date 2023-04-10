module Donation.Types
  ( module Donation.Types.Beneficiary
  , module Donation.Types.Deadline
  , ContractResult
  , TransactionId
  , Common
  , Donate
  , Reclaim
  , Address
  , Value
  )
  where

import Contract.Address as CA
import Contract.Transaction as CT
import Data.BigInt as DBI
import Donation.Types.Beneficiary
import Donation.Types.Deadline

type TransactionId = CT.TransactionHash
type Value = DBI.BigInt
type Address = CA.Address
type Common = ( beneficiary :: Beneficiary )
type Donate = 
  { value :: Value
  , deadline :: Deadline
  | Common
  }
type Reclaim =
  { donationTxId :: TransactionId
  | Common
  }
type ContractResult =
  { txId :: TransactionId
  , txFinalFee :: Value
  }
