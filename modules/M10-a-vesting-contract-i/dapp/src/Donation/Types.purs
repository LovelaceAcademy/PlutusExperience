module Donation.Types where

import Contract.Address as CA
import Contract.Transaction as CT
import Contract.Time as CTi
import Contract.PlutusData as CPD
import Contract.Numeric.BigNum as CNBN
import Data.BigInt as DBI

type ContractResult =
  { txId :: CT.TransactionHash
  , txFinalFee :: DBI.BigInt
  }
type TransactionId = CT.TransactionHash

type DonateParams = 
  { value :: DBI.BigInt
  , beneficiary :: CA.Address
  , deadline :: CTi.POSIXTime
  }
type ReclaimParams =
  { beneficiary :: CA.Address
  , donationTxId :: TransactionId
  }

data VestingDatum = VestingDatum
    { beneficiary :: CA.PubKeyHash
    , deadline    :: CTi.POSIXTime
    }

--instance PlutusTx.IsData.Class.ToData VestingDatum where
--  {-# INLINABLE PlutusTx.IsData.Class.toBuiltinData #-}
--  PlutusTx.IsData.Class.toBuiltinData
--    (VestingDatum arg_a92l arg_a92m)
--    = (PlutusTx.Builtins.Internal.mkConstr 0)
--        ((PlutusTx.Builtins.Internal.mkCons
--            (PlutusTx.IsData.Class.toBuiltinData arg_a92l))
--           ((PlutusTx.Builtins.Internal.mkCons
--               (PlutusTx.IsData.Class.toBuiltinData arg_a92m))
--              (PlutusTx.Builtins.Internal.mkNilData
--                 PlutusTx.Builtins.Internal.unitval)))

instance CPD.ToData VestingDatum where
  toData (VestingDatum { beneficiary, deadline }) = CPD.Constr CNBN.zero
    [ CPD.toData beneficiary
    , CPD.toData deadline
    ]
