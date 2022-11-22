---
title: M04 - We should have a map
author: Walker Leite
---
# Introduction

## Getting Started

In this module we will introduce you to very important concepts of PureScript and Haskell: Typeclasses, Monoids and Functors

To run this presentation type (you will need [nix](https://nixos.org)):

```sh
../../slide README.md
```

### Community Support

- [LovelaceAcademy Discord](https://discord.gg/fWP9eGdfZ8)
- [StackExchange](https://cardano.stackexchange.com/) (:bulb: use the tag lovelace-academy)
- [Plutonomicon Discord](https://discord.gg/gGFdGaUE)

[Module video]

## What you should know

1. The EUTxO Model (module 1)
2. PureScript (modules 2-3)

# Type Classes and forall

## Before

```purescript
module Main where

import Prelude
import Data.Semigroup ((<>))
import Effect.Console (log)

ordinal :: Int -> String
ordinal n = let s = show n in case n of
              1 -> s <> "st"
              2 -> s <> "nd"
              3 -> s <> "rd"
              _ -> s <> "th"

welcome :: String -> String -> Int -> String
welcome w n m = let m' | m <4 = ordinal m
                       | otherwise = "invalid"
                    thing = "module"
                    sep :: String -> String
                    sep s = " " <> s
                in sep w <> sep m' <> sep thing <> sep "of" <> sep n

main = log $ welcome' 3 <> welcome' 4 where
  welcomeTxt :: String
  welcomeTxt = "welcome to the"
  course = "plutus experience"
  welcome' n = "\n" <> welcome welcomeTxt course n
```

## After

```purescript
module Main where

import Prelude (($))
import Data.Semigroup ((<>))
import Effect.Console (log)

class Show a where
  show :: a -> String

data Module = First | Second | Third | Fourth

instance Show Module where
  show First = "1st"
  show Second = "2nd"
  show Third = "3rd"
  show Fourth = "4rd"
  
instance Show Int where
  show 1 = show First
  show 2 = show Second
  show 3 = show Third
  show 4 = show Fourth
  show _ = "invalid"

welcome :: forall a. Show a => String -> String -> a -> String
welcome w n m = let m' = show m
                    thing = "module"
                    sep :: String -> String
                    sep s = " " <> s
                in sep w <> sep m' <> sep thing <> sep "of" <> sep n

main = log $ welcome' Third <> welcome' 4 where
  welcomeTxt :: String
  welcomeTxt = "welcome to the"
  course = "plutus experience"
  welcome' :: forall a. Show a => a -> String
  welcome' n = "\n" <> welcome welcomeTxt course n
```

# Breakthrough

## Exercise 

`buildTx` is a function that runs `Validator` for each UTxO in `Inputs` with the given `Redeemer`, an `ScriptContext` (having the `Outputs`) and a corresponding `Datum`; and returns `true` if all `Validator`s returns `true`.

```purescript
type Validator = Redeemer -> ScriptContext -> Datum -> Boolean

buildTx :: Inputs -> Outputs -> Redeemer -> Validator -> Boolean
```

Implement the `buildTx` function and all missing definitions.
