---
title: M02 - PureScript Foundations
author: Walker Leite
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

## PureScript

- Compile to readable JavaScript
- Similar Haskell/Plutus syntax (learn once, use twice)
- Same Haskell [purity](https://wiki.haskell.org/Functional_programming#Purity)
- [purs-nix](https://github.com/ursi/purs-nix)
- [Cardano-Transaction-Lib](https://github.com/Plutonomicon/cardano-transaction-lib)

[More on "Why PureScript"](https://jordanmartinez.github.io/purescript-jordans-reference-site/content/01-Getting-Started/01-Why-Learn-PureScript.html)

### Types

PureScript basic structure divides in two main elements: 

```purs
```

# Breakthrough: Plutus Emulator

> Run the default smart contract in the emulator

> Bonus: Run also docs

## Bootstrap

- [Enable IOG Cache](https://github.com/input-output-hk/plutus-apps/#iohk-binary-cache)

```bash
git clone https://github.com/input-output-hk/plutus-apps.git
git checkout 6b8bd0d80115ed5379ada31eea9e4e50db31f126
```

## Links

- [Online Plutus Docs](https://playground.plutus.iohkdev.io/doc)
