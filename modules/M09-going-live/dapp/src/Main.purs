module Main (main) where

import Contract.Prelude
  ( ($)
  , Effect
  , Unit
  , void
  )
import Contract.Monad (launchAff_, runContract)
import Contract.Config (testnetEternlConfig)
import Donation.Contract (donation)

main :: Effect Unit
main = launchAff_
  $ void
  $ runContract testnetEternlConfig
  $ donation
