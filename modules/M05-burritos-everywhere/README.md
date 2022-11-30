---
title: M05 - Burritos Everywhere
author: Walker Leite
---
# Introduction

## Getting Started

In this module we will see more fundamental type classes of PureScript and Haskell.

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

1. PureScript (modules 2-4)

# Breakthrough

## Exercise 

In module 04 we've used `buildTx` to simulate a transaction build, in this module we'll simulate a Contract behavior using what we've learned so far.

```purescript
type Validator = Redeemer -> ScriptContext -> Datum -> Boolean

data Contract a = Contract a

buildTx :: Inputs -> Outputs -> Redeemer -> Validator -> Contract Boolean

runContract :: forall a. Contract a -> Effect a
```

Change previous created `buildTx` to return the value using the `Contract` monad and implement the missing definitions. `runContract` must log in the console the contract execution. 
