module Wallet (component) where

import Prelude (($), unit, pure, bind, discard)
import Control.Monad.State (get, modify_)
import Data.Maybe (Maybe (Nothing, Just))
import Effect.Class (class MonadEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Wallet.Api (Cbor, Api)
import Wallet.Api.Aff (getBalance)

type State = { api :: Api, balance :: Maybe Cbor }

component :: forall q o m.  MonadEffect m => MonadAff m => H.Component q Api o m
component = H.mkComponent {
  initialState: \api -> { api, balance: Nothing },
  eval: case _ of
    (H.Initialize a) -> do
      { api } <- get
      balance <- liftAff $ getBalance api
      modify_ $ _ { balance = Just balance }
      pure a
    (H.Finalize a) -> pure a
    (H.Receive _ a) -> pure a
    (H.Action _ a) -> pure a
    (H.Query _ fn) -> pure $ fn unit
  , render: case _ of
    { balance: (Just value) } -> HH.div_ [
      HH.span_ [HH.text "Balance: "],
      HH.span_ [HH.text  value]
    ]
    _ -> HH.div_ [
      HH.text "Loading balance..."
    ]
}
