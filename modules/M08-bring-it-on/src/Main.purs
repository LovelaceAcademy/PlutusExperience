module Main (main) where

import Contract.Prelude

import Contract.Config (testnetConfig)
import Donation (component)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (component testnetConfig) unit body
