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
4. Visitor integrate its wallet
5. Visitor grabs all ADA

## Prerequisites

### TODO: Nami wallet on Preview with covering funds

- Download and install [Nami](https://namiwallet.io) in your browser
- TODO: Change Nami to preview network (14-09-17-34)
    tx: 8509f83a40b43af4fc2639ce2404a7be50d569b883c780569fb94fab5ce047c3
- Send test ADA to your wallet in [Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet)

## Donator integrates his wallet

(14-09-17-53)

```bash
cd episodes/08-bring-it-on
nix flake init -t github:Plutonomicon/cardano-transaction-lib
git init
git commit -a -m "Initial commit"
```
