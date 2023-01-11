module Main where

import Prelude (Unit, bind, unit)

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Wallet (component)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
