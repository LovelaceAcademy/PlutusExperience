module Main (main) where

import Contract.Prelude
  ( ($)
  , Effect
  , Unit
  , Maybe(Just)
  , Tuple(Tuple)
  , bind
  )

import Contract.Config (testnetEternlConfig)
import Effect.Aff (Aff)
import Foreign.Object as FO
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Storybook as HS
import Minting as M

stories :: HS.Stories Aff
stories = FO.fromFoldable
  [ Tuple "Mint" $ HS.proxy (M.mintPage testnetEternlConfig)
  ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  HS.runStorybook { stories, logo: Just $ HH.text "Non Fungible Tokens" } body
