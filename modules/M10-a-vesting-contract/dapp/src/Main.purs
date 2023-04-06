module Main (main) where

import Contract.Prelude
  ( ($)
  , Effect
  , Unit
  , void
  )
import Contract.Monad (launchAff_, runContract)
import Contract.Config (testnetEternlConfig)
import Data.BigInt as DBI
import Donation.Contract (donate)

main :: Effect Unit
main = launchAff_
  $ void
  $ runContract testnetEternlConfig
  $ donate $ DBI.fromInt 10_000_000
