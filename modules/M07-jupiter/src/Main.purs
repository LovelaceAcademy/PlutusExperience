module Main where

import Prelude (Unit, bind)

import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Wallet (component)
import Wallet.Api.Aff (enableWallet)
import Web.HTML (window)

main :: Effect Unit
main = HA.runHalogenAff do
  w <- liftEffect window
  api <- enableWallet w "nami"
  body <- HA.awaitBody
  runUI component api body
