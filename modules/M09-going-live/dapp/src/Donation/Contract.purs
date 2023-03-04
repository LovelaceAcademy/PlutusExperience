module Donation.Contract (contract) where

import Contract.Prelude
  ( (<<<)
  , (=<<)
  , Unit
  , show
  )
import Contract.Address (ownPaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Donation.Script

contract :: Contract () Unit
contract = logInfo' <<< show =<< ownPaymentPubKeyHash
