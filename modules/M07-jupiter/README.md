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

In this module we'll see Row Types and Records, monad lifting, async computations (Aff) and we'll build a wallet webapp using Halogen.

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

## Remembering Kinds

```purescript
module Main where

import Prelude (($), Unit, show, discard)
import Effect (Effect)
import Effect.Console (log)

-- concrete type
type Name :: Type
type Name = String

-- concrete type
data Dog :: Type
data Dog = Dog Name

-- high kinded type
data Maybe :: Type -> Type
data Maybe a = Nothing | Just a

type MaybeDog :: Type
type MaybeDog = Maybe Dog

mayShowDog :: MaybeDog -> String
mayShowDog Nothing = show "No dog :("
mayShowDog (Just (Dog name)) = show name

main :: Effect Unit
main = do
    log $ mayShowDog Nothing
    log $ mayShowDog $ Just (Dog "Max")
```

## Problem

What if we need to have way more dog attributes beyond name?

We could use Sum Types:

```purescript
module Main where

import Prelude (($), (<>), Unit, show)
import Effect (Effect)
import Effect.Console (log)

type Name = String
type Owner = Name
type Age = Int
data Dog = Dog Name Age Owner

showDog :: Dog -> String
showDog (Dog name age owner) =
     show name
  <> ", "
  <> show age
  <> ", "
  <> show owner

main :: Effect Unit
main = log $ showDog $ Dog "Max" 2 "John"
```

It's a bit clunky but it works, what would be a better alternative?

## Row Kind

```purs
data Row :: a -> Type
data Row a = ( label :: a )

type Dog :: Row Type
type Dog = ( name :: String, age :: Int, owner :: String )

-- ( label :: a ) is the type constructor for Row at language level ;)
-- name, age and owner are so called "indexed-labels"
-- !! because `Row Type` we can only have concrete types in labels) !!
```

> :bulb: You can see a `Row Type` like instructions on how to create a dog definition.

Great! But having a type that can't be used to build definitions is not so useful. How can I build a type and a value of `Dog` `Type`?

## Records

```purs
data Record :: Row Type -> Type
```

`Record` constructor: Given a defined `Row Type`, give me a corresponding concrete `Type`

```purs
type Dog :: Type
type Dog = Record ( name :: String, age :: Int, owner :: String )

-- like Row Type, Record has a syntax suggar constructor { }
type Dog :: Type
type Dog = { name :: String, age :: Int, owner :: String }
```

```purescript
module Main where

import Prelude (($), (<>), Unit, show)
import Effect (Effect)
import Effect.Console (log)

type Dog = { name :: String, age :: Int, owner :: String }

showDog :: Dog -> String
showDog dog@{ name, owner: ownerName } =
     show name
  <> ", "
  <> show dog.age
  <> ", "
  <> show ownerName

main :: Effect Unit
main = log $ showDog
  { name: "Max"
  , age: 2
  , owner: "John"
  }
```

## Optional field problem 1

What if `owner` is optional?

```purs
main :: Effect Unit
main = do
  log $ showDog
    { name: "Max"
    , age: 2
    , owner: "John"
    }
  log $ showDog
    { name: "Lily"
    , age: 3
    }
```

```
  Type of expression lacks required label owner.

while checking that expression { name: "Lily"
                               , age: 3
                               }
  has type { age :: Int
           , name :: String
           , owner :: String
           }
while applying a function showDog
  of type { age :: Int
          , name :: String
          , owner :: String
          }
          -> String
  to argument { name: "Lily"
              , age: 3
              }
in value declaration main
```


## Optional field problem 2

What if `showOwner` needs handle only owner?

```purs
type Dog = { name :: String, age :: Int, owner :: String }
type Ownership = { owner :: String }

--...

showOwner ::  Ownership -> String
showOwner { owner } = show owner

main :: Effect Unit
main = let
  dog = { name: "Max"
        , age: 2
        , owner: "John"
        }
  in do
    log $ showDog dog
    log $ showOwner dog
```

```
  Type of expression contains additional label age.

while checking that type { age :: Int
                         , name :: String
                         , owner :: String
                         }
  is at least as general as type { owner :: String
                                 }
while checking that expression dog
  has type { owner :: String
           }
in value declaration main
```

## Open / Closed Rows

Closed Row:

```purs
type Dog :: Row Type
type Dog = ( name :: String, age :: Int, owner :: String )
```

Open Row:

```purs
type Dog :: Row Type -> Row Type
type Dog a = ( name :: String, age :: Int | a )
```

> :bulb: Dog is a high kinded type, it needs `a`, which must have kind `Row Type`, to define its concrete `Row Type`.

```purs
type Ownership :: Row Type
type Ownership = ( owner :: String )

type Dog :: Row Type -> Row Type
type Dog a = ( name :: String, age :: Int | a ) 

type DogWithOwner :: Row Type
type DogWithOwner = Dog Ownership
```

> :bulb: Remember that a `Row Type` is the instruction on how to build a `Type` using `Record :: Row Type -> Type`.

## Open / Closed Records

Closed Record:

```purs
type Dog :: Type
type Dog = { name :: String, age :: Int, owner :: String }
```

Open Record:

```purs
type Dog :: Row Type -> Type
type Dog a = { name :: String, age :: Int | a }
```

> :bulb: Dog is a high kinded type, it needs `a`, which must have kind `Row Type`, to define its concrete `Type`

```purs
type Ownership :: Row Type
type Ownership = ( owner :: String )

type Dog :: Row Type -> Type
type Dog a = { name :: String, age :: Int | a }

type DogWithOwner :: Type
type DogWithOwner = Dog Ownership
```

`DogWithOwner` is the concrete `Type` of a dog, result of applying `Ownership` to the `Dog` HKT, which apply the received `Ownership` to `Record :: Row Type -> Type` as first argument.

In other words, we use the `Ownership` `Row Type` to tell `Dog` how to build a `Type` using `Record` type constructor.

## Solution

```purescript
module Main where

import Prelude (($), (<>), Unit, show, discard)
import Effect (Effect)
import Effect.Console (log)

type Ownership :: Row Type
type Ownership = ( owner :: String )

type Dog :: Row Type -> Type
type Dog a = { name :: String, age :: Int | a }

type DogWithOwner :: Type
type DogWithOwner = Dog Ownership

showDog :: forall a. Dog a -> String
showDog { name, age } = show name <> ", " <> show age

showOwner :: forall a. { owner :: String | a } -> String
showOwner { owner } = show owner

main :: Effect Unit
main = let
  dog1 = { name: "Max"
         , age: 2
         , owner: "John"
         }
  dog2 = { name: "Lily"
         , age: 3
         }
  in do
    log $ showDog dog1
    log $ showDog dog2
    log $ showOwner dog1
    -- uncomment to throw "lacks required label owner" 
    -- log $ showOwner dog2
```

## Links

- [Jordan's Reference - Records](https://jordanmartinez.github.io/purescript-jordans-reference-site/content/11-Syntax/01-Basic-Syntax/src/05-Records)

# Lifting and async

## Problem

Remember the `Contract` monad from module 5?

```purs
buildTx :: Inputs -> Outputs -> Redeemer -> Validator -> Contract Boolean

data Contract a = Contract a
```

What if we need to produce a side-effect in buildTx?

```purs
data Contract a = Contract (Effect a) 

runContract :: forall a. Contract a -> Effect a
runContract (Contract eff) = log ">running effects" *> eff
```

So we could `runContract buildTx`, right? Well, we can't:

```purs
buildTx :: Contract Boolean
buildTx = do
  log ">building tx"
  pure true
```

```
Could not match type

    Effect

  with type

    Contract
```

It happens because log returns `Effect`, not `Contract`.

## Solution

![jupiter](images/001.gif)

## Solution - MonadEffect

```purescript
module Main where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)

data Contract a = Contract (Effect a)

derive instance Functor Contract
instance Apply Contract where
  apply = ap
instance Bind Contract where
  --bind :: forall a b. m a -> (a -> m b) -> m b
  bind (Contract eff) f = Contract $ eff >>= \v -> case f v of
    Contract eff' -> eff'
instance Applicative Contract where
  pure value = Contract (pure value)
instance Monad Contract

instance MonadEffect (Contract) where 
  --liftEffect :: forall m a. MonadEffect m => Effect a -> m a
  liftEffect = Contract

c1 :: Contract Boolean
c1 = do
  liftEffect $ log ">running contract c1"
  pure true

c2 :: Contract Boolean
c2 = do
  liftEffect $ log ">running contract c2"
  pure false

runContract :: forall a. Contract a -> Effect a
runContract (Contract eff) = log ">running effects" *> eff

main :: Effect Unit
main = do
  result1 <- runContract c1
  log $ ">result1: " <> show result1
  result2 <- runContract c2
  log $ ">result2: " <> show result2
```

## Aff 

Aff is used to represent asynchronous  effects (similar to Promises, but with more features)

```purs
-- The computation may either error with an exception, or produce a result of type a.
data Aff a

```

To launch it we use `launchAff_ :: forall a. Aff a -> Effect Unit`

```purescript
module Main where

import Prelude

import Data.Time.Duration (Milliseconds (Milliseconds))
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Effect.Console (log, logShow)
import Effect.Class (liftEffect)
import Fetch (fetch)

main :: Effect Unit
main = do
  log "This is an Effect computation (Effect monadic context)"

  launchAff_ do
    { text } <- fetch "https://httpbin.org/uuid" {}
    uuid <- text
    liftEffect $ logShow uuid

  launchAff_ do
    liftEffect $ log "[other aff] start"
    delay $ Milliseconds 1000.0
    liftEffect $ log "[other aff] done"
    
  log "Program finished before async stuff ;)"
```

## Aff instances


```purs
-- Relevant instances, beyond Functor, Apply, Applicative, Bind and Monad

MonadThrow Error Aff
-- so it can throwError
MonadError Error Aff
-- so we can catchError
MonadEffect Aff
-- so we can liftEffect

-- MonadAff, like MonadEffect, allows one yo lift from Aff to m
class (MonadEffect m) <= MonadAff m
  liftAff :: Aff ~> m
```

## Links

- [Jordan's Reference - MonadEffect](https://jordanmartinez.github.io/purescript-jordans-reference-site/content/21-Hello-World/02-Effect-and-Aff/src/03-Aff/02-Lifting-Monads/01-MonadEffect.html#monadeffect-1)
- [Jordan's Reference - Aff](https://jordanmartinez.github.io/purescript-jordans-reference-site/content/21-Hello-World/02-Effect-and-Aff/src/03-Aff/index.html)

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
nix flake init -t github:LovelaceAcademy/nix-templates#pix
```

## References and Links

- [CIP-0030](https://github.com/cardano-foundation/CIPs/blob/6f50596b99b7de9ff8601553762566fde0ef7a30/CIP-0030/README.md)
- [cardano-serialization-lib](https://github.com/Emurgo/cardano-serialization-lib)
- [purescript-cip30](https://github.com/anton-k/purescript-cip30)
- [purescript-cardano-serialization-lib](https://github.com/anton-k/purescript-cardano-serialization-lib)
- [Jordan's Reference - FFI](https://jordanmartinez.github.io/purescript-jordans-reference-site/content/11-Syntax/02-Foreign-Function-Interface)
