module Main (main) where

import Contract.Prelude
  ( ($)
  , Effect
  , Unit
  , void
  , bind
  )
import Contract.Monad (launchAff_, runContract)
import Contract.Config (testnetEternlConfig)
import Contract.Chain (currentTime)
import Data.BigInt as DBI
import Donation (ownWalletAddress, donate)

main :: Effect Unit
main = launchAff_
  $ void
  $ runContract testnetEternlConfig do
     donator <- ownWalletAddress "donator"
     deadline <- currentTime
     donate { value : DBI.fromInt 10_000_000
            , beneficiary: donator
            , deadline
            }
