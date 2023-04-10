module Main (main) where

import Contract.Prelude
  ( ($)
  , Effect
  , Unit
  , Maybe(Just)
  , Tuple(Tuple)
  , void
  , bind
  )

import Contract.Config (testnetEternlConfig)
import Effect.Aff (Aff)
import Foreign.Object as FO
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Storybook as HS
import Donation as D

stories :: HS.Stories Aff
stories = FO.fromFoldable
  [ Tuple "Donate" $ HS.proxy (D.donatePage testnetEternlConfig)
  , Tuple "Reclaim" $ HS.proxy (D.reclaimPage testnetEternlConfig)
  ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  HS.runStorybook { stories, logo: Just $ HH.text "A Vesting Contract" } body
