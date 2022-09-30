module Main (main) where

import Contract.Prelude

import Contract.Config (testnetConfig)
import Contract.Monad (launchAff_, runContract)
import Donation (contract)


main :: Effect Unit
main = launchAff_ $ runContract testnetConfig contract
