module Donation (contract) where

import Contract.Prelude

import Contract.Monad (Contract)
import Donation.Script (validator)

contract :: Contract () Unit
contract = do
  v <- liftEither $ validator
  log "loaded"
