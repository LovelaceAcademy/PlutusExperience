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

[Episode video]

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


```markdown
Frontend:

(Browser) Wallet - CTL - CSL
                    |
Backend:            |
                    |
Ogmius (WebSocket) -|- Ogmius Datum Cache
                    |
             Plutus Server
```

- [CTL](https://github.com/Plutonomicon/cardano-transaction-lib) is a Purescript library/interface for building smart contract transactions on Cardano.
- [CSL](https://github.com/Emurgo/cardano-serialization-lib) is a [WASM](https://developer.mozilla.org/en-US/docs/WebAssembly) library to serialize and unserialize plutus compatible data. We don't use it directly.
- [Ogmius](https://github.com/CardanoSolutions/ogmios) is a service to query the blockchain using websocket protocol. We don't use it directly.
- [Ogmius Datum Cache](https://github.com/mlabs-haskell/ogmios-datum-cache) is a service to store datum values for V1 (non-vasil) contracts. We don't use it directly.
- [Plutus Server](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/server): when using paramaterized contracts, Haskell is still required to build the validator using its meta-programming capabilities, this server is used for this.

## Scenario

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
