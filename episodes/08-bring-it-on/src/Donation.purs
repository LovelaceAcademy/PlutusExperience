module Donation (contract) where

import Contract.Prelude

import Contract.Monad (Contract)

contract :: Contract () Unit
contract = do
  log "loaded"
