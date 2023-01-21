---
title: M07 - Jupiter
author: Walker Leite
patat:
  eval:
    purescript:
      command: purs-eval | node --experimental-network-imports --input-type module | prettier --parser html
---
# Introduction

## Getting Started

In this module we'll see Row Types and Records, monad transformers, async computations (Aff) and we'll build a wallet webapp using Halogen.

To run this presentation type (you will need [nix](https://nixos.org)):

```sh
../../slide README.md
```

### Community Support

- [LovelaceAcademy Discord](https://discord.gg/fWP9eGdfZ8)
- [StackExchange](https://cardano.stackexchange.com/) (:bulb: use the tag lovelace-academy)
- [Plutonomicon Discord](https://discord.gg/gGFdGaUE)

[Module video] - Coming Soon...

## What you should know

1. Nix (module 1)
2. Nix flakes (module 3)
3. PureScript (modules 2-5)

# Row Types and Records

# Monad Transformers

# Aff

# Halogen

## Why

We need to build simple user interfaces using the knowledge acquired so far.

Halogen is a type-safe library for building user interfaces in PureScript, it rely on both a well known architecture (Elm) and PureScript features like Row Types and Monad Transformers.

Some alternatives:

- [react-basic](https://pursuit.purescript.org/packages/purescript-react-basic) core types and tools for React;
- [deku](https://purescript-deku.netlify.app) FRP UI framework for games and web apps;
- [jelly](https://jelly.yukikurage.net) framework for building web applications based on Signal;

## HTML 

### Hello World

In the module 3 we've created types and functions to help us render HTML, Halogen does the same, in the `Halogen.HTML`:

```purescript
module Main where

import Prelude
import Data.Newtype (unwrap)
import Effect.Console (log)
import Halogen.HTML as HH
import Halogen.VDom.DOM.StringRenderer as HVDS

html = HH.h1 [ ] [ HH.text "Hello, world" ]

logRender = log <<< HVDS.render (const mempty) <<< unwrap
main = logRender html
```

### Form Name

```purescript
module Main where

import Prelude ((<<<), const, mempty)
import Data.Newtype (unwrap)
import Effect.Console (log)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.StringRenderer (render)

html :: forall w i. HH.HTML w i
html = HH.div
    [ HP.id "root" ]
    [ HH.input
        [ HP.placeholder "Name" ]
    , HH.button
        [ HP.classes [ HH.ClassName "btn-primary" ]
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Submit" ]
    ]

logRender = log <<< render (const mempty) <<< unwrap
main = logRender html
```

## Components

> Halogen HTML is one basic building block of Halogen applications. But pure functions that produce HTML lack many essential features that a real world application needs: state that represents values over time, effects for things like network requests, and the ability to respond to DOM events (for example, when a user clicks a button).

```purs
module Main where

import Prelude

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver as HD

data Action = Increment | Decrement

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.text (show state)
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      ]

  handleAction = case _ of
    Decrement ->
      H.modify_ \state -> state - 1

    Increment ->
      H.modify_ \state -> state + 1

main = HA.runHalogenAff do
  body <- HA.awaitBody
  HD.runUI component unit body
```

## Components - Zooming in

### Main

```purs
HA.runHalogenAff :: forall x. Aff x -> Effect Unit
HA.awaitBody :: Aff HTMLElement

-- q: query, i: input, o: output

HD.runUI :: forall q i o. Component q i o Aff -> i -> HTMLElement -> Aff (HalogenIO q o Aff)

type HalogenIO q o m =
  { dispose :: m Unit
  , messages :: Emitter o
  , query :: forall a. q a -> m (Maybe a)
  }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  HD.runUI component unit body
```

### mkComponent

```purs
-- s: state, q: query, a: action, sl: slot, i: input, o: output, m: monad

mkComponent :: forall s q a sl i o m. ComponentSpec s q a sl i o m -> Component q i o m

type ComponentSpec s q a sl i o m =
  { eval :: (HalogenQ q a i) ~> (HalogenM s a sl o m)
  , initialState :: i -> s
  , render :: s -> HTML (ComponentSlot sl m a) a
  }
```

### mkEval and defaultEval

```purs
-- s: state, q: query, a: action, sl: slot, i: input, o: output, m: monad


-- ~> is a Natural Transformation, don't worry about it
--    you can safely read it as a normal arrow (->).
mkEval :: forall s q a sl i o m. EvalSpec s q a sl i o m -> (HalogenQ q a i) ~> (HalogenM s a sl o m)

type EvalSpec s q a s i o m =
  { finalize :: Maybe a
  , handleAction :: a -> HalogenM s a sl o m Unit
  , handleQuery :: forall a. q a -> HalogenM s a sl o m (Maybe a)
  , initialize :: Maybe a
  , receive :: i -> Maybe a
  }

defaultEval :: forall s q a sl i o m. EvalSpec s q a sl i o m

H.mkComponent
  { initialState
  , render
  , eval: H.mkEval (H.defaultEval { handleAction = ?handleAction })
  }
```

> :bulb: Looking in mkEval source code might help you understand what is happening behind the scene, but you don't need to understand it to use Halogen

### HalogenM

```purs
-- s: state, q: query, a: action, sl: slot, i: input, o: output, m: monad

newtype HalogenM s a sl o m a = HalogenM (Free (HalogenF s a sl o m) a)

-- In links section there is a long video about Halogen and the Free type
-- not required to use Halogen, but still very interesting

-- Relevant instances, beyond Functor, Apply, Applicative, Bind and Monad

(Monoid a) => Monoid (HalogenM s a sl o m a)
-- so we can append many handleActions/handleQueries

(MonadEffect m) => MonadEffect (HalogenM s a sl o m)
-- so we can liftEffect to perform computations and side-effects

(MonadAff m) => MonadAff (HalogenM s a sl o m)
-- so we can liftAff to perform async computations and side-effects

MonadState s (HalogenM s a sl o m)
-- so we can get/put/modify s
```

## Aff Example

```purs
handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  SetUsername username -> do
    H.modify_ _ { username = username, result = Nothing }

  MakeRequest event -> do
    username <- H.gets _.username
    H.modify_ _ { loading = true }
    response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
    H.modify_ _ { loading = false, result = map _.body (hush response) }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.form
    [ HE.onSubmit \ev -> MakeRequest ev ]
    [ HH.h1_ [ HH.text "Look up GitHub user" ]
    , HH.label_
        [ HH.div_ [ HH.text "Enter username:" ]
        , HH.input
            [ HP.value st.username
            , HE.onValueInput \str -> SetUsername str
            ]
        ]
    , HH.button
        [ HP.disabled st.loading
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Fetch info" ]
    , HH.p_
        [ HH.text $ if st.loading then "Working..." else "" ]
    , HH.div_
        case st.result of
          Nothing -> []
          Just res ->
            [ HH.h2_
                [ HH.text "Response:" ]
            , HH.pre_
                [ HH.code_ [ HH.text res ] ]
            ]
    ]
```

## References and Links

- [Halogen Guide](https://purescript-halogen.github.io/purescript-halogen)
- [PS Unscripted - Free From Tree & Halogen VDOM](https://www.youtube.com/watch?v=eKkxmVFcd74&t=7143s)

# Breakthrough

## Exercise 

 > User Story: As a cardano user I want to see all my available funds in my browser wallet

## Bootstrap

```bash
nix flake init -t github:LovelaceAcademy/nix-templates#purs-nix
```

## References and Links

- [CIP-0030](https://github.com/cardano-foundation/CIPs/blob/6f50596b99b7de9ff8601553762566fde0ef7a30/CIP-0030/README.md)
- [cardano-serialization-lib](https://github.com/Emurgo/cardano-serialization-lib)
- [purescript-cip30](https://github.com/anton-k/purescript-cip30)
- [purescript-cardano-serialization-lib](https://github.com/anton-k/purescript-cardano-serialization-lib)
- [Jordan's Reference - FFI](https://jordanmartinez.github.io/purescript-jordans-reference-site/content/11-Syntax/02-Foreign-Function-Interface)
