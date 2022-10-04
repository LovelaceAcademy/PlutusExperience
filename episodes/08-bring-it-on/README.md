---
title: 08 - Bring it on
author: Walker Leite
patat:
  eval:
    hs:
      command: xargs -0 ghc -e
---
# Introduction

## Getting Started

In this episode we'll build the most common off-chain code for a smart contract on Cardano.

To run this presentation type (you will need [nix](https://nixos.org)):

```bash
../../slide README.md
```

## Requisites

1. Plutus
    - You should be familiar with [EUTxO Model](https://github.com/LovelaceAcademy/plutus-experience#episode-1---plutus-playground);
    - You should be able to [serialize plutus code](https://github.com/LovelaceAcademy/plutus-experience#episode-7---going-live)
2. Nix
    - You should be familiar with [Nix Language](https://nixos.org/manual/nix/stable/language);
    - You should be familiar with [Nix Flake](https://github.com/LovelaceAcademy/plutus-experience#episode-3---our-first-web-app);
3. PureScript
    - You should be familiar with [Monad Transformers];
    - You should be familiar with [Halogen];


## Cardano Transaction Lib

TODO

## Building a fancy UI

> User Story: As a donator I want to give any amount of ADAs to the first visitor

### Steps

1. Donator integrates his wallet
2. Donator locks `n` ADA into `Script Address`
3. Visitor see avaliable `n` ADA 
4. Visitor integrates his wallet
5. Visitor grabs all ADA

### CLI wallet on PreProd with covering funds

- Create `donator.{skey,vkey,addr}` with given `cardano-cli`
- Create `visitor.{skey,vkey,addr}` with given `cardano-cli`
- Send test ADA to donator address [Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet)

## Donator integrates his wallet

```bash
cd episodes/08-bring-it-on
nix flake init -t github:Plutonomicon/cardano-transaction-lib
git init
git commit -a -m "Initial commit"
```
