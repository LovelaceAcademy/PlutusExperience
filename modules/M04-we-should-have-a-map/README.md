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

# Breakthrough

## Exercise 

`buildTx` is a function that runs `Validator` for each UTxO in `Inputs` with the given `Redeemer`, an `ScriptContext` (having the `Outputs`) and a corresponding `Datum`; and returns `true` if all `Validator`s returns `true`.

```purescript
type Validator = Redeemer -> ScriptContext -> Datum -> Boolean

buildTx :: Inputs -> Outputs -> Redeemer -> Validator -> Boolean
```

Implement the `buildTx` function and all missing definitions.
