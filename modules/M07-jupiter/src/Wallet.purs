module Wallet (component) where

import Prelude (($), unit, pure)
import Data.Function (const)
import Halogen as H
import Halogen.HTML as HH

component :: forall q o m b. H.Component q b o m
component = H.mkComponent {
  initialState: const unit,
  eval: case _ of
    (H.Initialize x) -> pure x
    (H.Finalize x) -> pure x
    (H.Receive _ x) -> pure x
    (H.Action _ x) -> pure x
    (H.Query _ fn) -> pure $ fn unit,
  render: const $ HH.div_ [
    HH.text "Hello World!"
  ]
}
