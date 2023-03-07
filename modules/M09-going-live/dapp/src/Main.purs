module Main (main) where

import Contract.Prelude
  ( ($)
  , (<>)
  , Effect
  , Unit
  , void
  , show
  , bind
  , discard
  )
import Contract.Monad (launchAff_, runContract)
import Contract.Config (testnetEternlConfig)
import Contract.Log (logInfo')
import Donation.Contract (donation, reward)

main :: Effect Unit
main = launchAff_
  $ void
  $ runContract testnetEternlConfig do
     { txId } <- donation
     logInfo' $ "donation done: " <> show txId
     { txId: txId' } <- reward txId
     logInfo' $ "reward done: " <> show txId'
