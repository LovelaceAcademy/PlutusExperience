module Wallet (component) where

import Prelude (($), (<$>), (+), unit, pure, bind, discard, show)
import Control.Monad.State (get, modify_)
import Data.Maybe (Maybe (Nothing, Just))
import Data.Foldable (foldM)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Wallet.Api as WA
import Wallet.Api.Aff as WAA

type State = { api :: WA.Api, balance :: Maybe Number }

component :: forall q o m.  MonadEffect m => MonadAff m => H.Component q WA.Api o m
component = H.mkComponent {
  initialState: \api -> { api, balance: Nothing },
  eval: case _ of
    (H.Initialize a) -> do
      { api } <- get
      utxos <- liftAff $ WAA.getUtxos api
      balance <- liftEffect $ foldM (\acc utxo -> (+) acc <$> (WA.coin $ WA.amount $ WA.output utxo)) 0.0 utxos
      modify_ $ _ { balance = Just balance }
      pure a
    (H.Finalize a) -> pure a
    (H.Receive _ a) -> pure a
    (H.Action _ a) -> pure a
    (H.Query _ fn) -> pure $ fn unit
  , render: case _ of
    { balance: (Just value) } -> HH.div_ [
      HH.span_ [HH.text "Balance: "],
      HH.span_ [HH.text $ show value]
    ]
    _ -> HH.div_ [
      HH.text "Loading balance..."
    ]
}
