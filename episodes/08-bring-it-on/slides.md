---
title: 08 - Bring it on
author: Walker Leite
patat:
  eval:
    hs:
      command: xargs -0 ghc -e
---
# Off-chain code

## Cardano Transaction Lib

TODO: CTL Slides

## Building a fancy UI

> User Story: As a donator I want to give any amount of ADAs to the first visitor

### Steps

1. Donator integrates his wallet
2. Donator locks `n` ADA into `Script Address`
3. Visitor see avaliable `n` ADA 
4. Visitor integrates his wallet
5. Visitor grabs all ADA

## Prerequisites

### CLI wallet on PreProd with covering funds

- Create `payment.{skey,vkey,addr}` with given cardano-cli
- Send test ADA to your wallet in [Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet)

## Donator integrates his wallet

```bash
cd episodes/08-bring-it-on
nix flake init -t github:Plutonomicon/cardano-transaction-lib
git init
git commit -a -m "Initial commit"
```
