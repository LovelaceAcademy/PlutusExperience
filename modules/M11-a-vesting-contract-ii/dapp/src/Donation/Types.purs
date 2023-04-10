module Donation.Types
  ( module Donation.Types.Beneficiary
  , module Donation.Types.Deadline
  , module Donation.Types.Value
  , ContractResult
  , TransactionId
  , Common
  , Donate
  , Reclaim
  , Address
  )
  where

import Contract.Address as CA
import Contract.Transaction as CT
import Data.BigInt as DBI
import Donation.Types.Beneficiary
import Donation.Types.Deadline
import Donation.Types.Value

type TransactionId = CT.TransactionHash
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
