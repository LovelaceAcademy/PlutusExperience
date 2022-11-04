---
title: M02 - PureScript Foundations
author: Walker Leite
patat:
  eval:
    purescript:
      command: purs-eval | node --experimental-network-imports --input-type module
---
# Introduction

## Getting Started

In this module we will introduce you to PureScript language, and we'll play in Try PureScript. 

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

1. A programming language
    - Although not essential, being familiar with a language will be beneficial in understanding the in-depth content of this course;
    - Knowing JavaScript and HTML will be beneficial;
2. Functional programming paradigm
    - Although not essential, it will help in the understanding of the in-depth content of this course;

## The problem

- We need to build smart contracts
- We need to run smart contracts on the front-end
- We need to integrate with other tools (wallets, libraries, and more)

## Alternatives

- Haskell (GHCJS)
- JavaScript
- TypeSCript
- Elm or Reason
- PureScript

# PureScript

## Why

- Compile to readable JavaScript
- Similar Haskell/Plutus syntax (learn once, use twice)
- Same Haskell [purity](https://wiki.haskell.org/Functional_programming#Purity)
- [purs-nix](https://github.com/ursi/purs-nix)
- [Cardano-Transaction-Lib](https://github.com/Plutonomicon/cardano-transaction-lib)

[More on "Why PureScript"](https://jordanmartinez.github.io/purescript-jordans-reference-site/content/01-Getting-Started/01-Why-Learn-PureScript.html)

## Types and conditionals

```haskell
entity_name :: Optional Type Signature
entity_name = definition
```

> :bulb: We use types to allow the compiler to test the function application (usage) and throw errors when something unexpected is being passed.

```purescript
module Main where

import Prelude
import Effect.Console (log)

adult :: Int
adult = 18

age :: Int -> String
age n = if n >= adult then "you're an adult" else "you're a child"

main = log (age 19)
```

## Type error

If we pass wrong parameters the compiler will throw:

```
main = log (age "18")
```

```
  Could not match type

    String

  with type

    Int


while checking that type String
  is at least as general as type Int
while checking that expression "18"
  has type Int
in value declaration main
```

> :bulb: It helps to read this error message from bottom-up

## Currying and Pattern matching

```purescript
module Main where

import Prelude
import Data.String (length)
import Effect.Console (log)

mkQuestion :: String -> Char -> String
mkQuestion "abcD" 'D' = "congrats, your answer is correct"
mkQuestion "abCd" 'C' = "congrats, your answer is correct"
mkQuestion "aBcd" 'B' = "congrats, your answer is correct"
mkQuestion "Abcd" 'A' = "congrats, your answer is correct"
mkQuestion s 'A' | length s > 4 = "your question should have 4 alternatives"
mkQuestion _ _ = "sorry, your answer is incorrect"

evaluateQ1 :: Char -> String
evaluateQ1 = mkQuestion "abCd"

evaluateQ2 :: Char -> String
evaluateQ2 = mkQuestion "aBcdf"

-- ignore do for a moment, we'll introduce it later
main = do
  log (evaluateQ1 'C')
  log (evaluateQ2 'A')
  log (mkQuestion "Abcd" 'B')
```

## Custom Types

How we create custom types?

```purescript
module Main where

import Prelude
import Effect.Console (log)

-- read | as or
data Animal = Cat String | Dog String

-- append :: String -> String -> String
talk :: Animal -> String
talk (Dog name) = append "Hello, I am the dog " name
talk (Cat name) = append "Hello, I am the cat " name

dog :: Animal
dog = Dog "max"

main = do
  log (talk dog)
  log (talk (Cat "kitty"))
```

## Custom Types - continuation

What if I need to identify animals by `Char` or `String`?

```purescript
module Main where

import Prelude
import Effect.Console (log)
import Data.String.CodeUnits (singleton)

data ID = CharID Char | StringID String
data Animal a = Cat a | Dog a

-- singleton ::  Char -> String
merge :: String -> ID -> String
merge str (CharID char) = append str (singleton char)
merge str (StringID str') = append str str'

talk :: Animal ID -> String
talk (Dog id) = merge "Hello, I am the dog " id
talk (Cat id) = merge "Hello, I am the cat " id

dog :: Animal ID
dog = Dog (StringID "max")

cat :: Animal ID
cat = Cat (CharID 'A')

main = do
  log (talk dog)
  log (talk cat)
```
