module Main (contract, main) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Config (testnetEternlConfig)
import Donation.Script

contract :: Contract () Unit
contract = logInfo' <<< show =<< ownPaymentPubKeyHash

main :: Effect Unit
main = launchAff_
  $ void
  $ runContract testnetEternlConfig
  $ contract
