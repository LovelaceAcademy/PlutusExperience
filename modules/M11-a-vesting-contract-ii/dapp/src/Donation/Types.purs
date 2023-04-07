module Donation.Types
  ( ContractResult
  , TransactionId
  , Common
  , Donate
  , Reclaim
  , Address
  , Deadline
  , Value
  )
  where

import Contract.Address as CA
import Contract.Transaction as CT
import Contract.Time as CTi
import Data.BigInt as DBI

type TransactionId = CT.TransactionHash
type Value = DBI.BigInt
type Deadline = CTi.POSIXTime
type Address = CA.Address
type Common = ( beneficiary :: Address )
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
