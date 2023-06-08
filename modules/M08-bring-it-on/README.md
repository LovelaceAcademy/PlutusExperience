---
title: M08 - Bring it on
author: Walker Leite
---
# Introduction

## Getting Started

In this module we'll build the most common off-chain code for a smart contract on Cardano.

To run this presentation type (you will need [nix](https://nixos.org)):

```bash
../../slide README.md
```

### Community Support

- [LovelaceAcademy Discord](https://discord.gg/fWP9eGdfZ8)
- [StackExchange](https://cardano.stackexchange.com/) (:bulb: use the tag lovelace-academy)
- [Plutonomicon Discord](https://discord.gg/gGFdGaUE)

[![Module Video](https://img.youtube.com/vi/YpJfFV_mcys/0.jpg)](https://www.youtube.com/watch?v=YpJfFV_mcys&list=PLHJ1yaDcSSacFJN2nMeahiSTxnWCTDLVr)

## Requisites

1. Plutus
    - You should be familiar with EUTxO Model (module 1);
    - You should be able to serialize plutus code (module 6);
2. Nix
    - You should be familiar with Nix Language (module 3);
    - You should be familiar with Nix Flakes (module 3);
3. PureScript
    - You should be familiar with Foreign Function Interface (module 7);
    - You should be familiar with MonadEffect, MonadAff (module 7);
    - You should be familiar with Halogen (module 7);

Optional, but recommended:

- [Monad Transforms](https://jordanmartinez.github.io/purescript-jordans-reference-site/content/21-Hello-World/05-Application-Structure/src/02-MTL/index.html) or [Monadic Adventures](https://book.purescript.org/chapter11.html).


# Cardano Transaction Lib

## Architecture

```markdown
Frontend:

(Browser) Wallet - CTL - CSL
                    |
Backend:            |
                    |
Ogmius (WebSocket) -|- Ogmius Datum Cache
                    |
                  Kopu 
```

- [CTL](https://github.com/Plutonomicon/cardano-transaction-lib) is a Purescript library/interface for building smart contract transactions on Cardano.
- [CSL](https://github.com/Emurgo/cardano-serialization-lib) is a [WASM](https://developer.mozilla.org/en-US/docs/WebAssembly) library to serialize and unserialize plutus compatible data. We don't use it directly.
- [Ogmius](https://github.com/CardanoSolutions/ogmios) is a service to query the blockchain using websocket protocol. We don't use it directly.
- [Ogmius Datum Cache](https://github.com/mlabs-haskell/ogmios-datum-cache) is a service to store datum values for V1 (non-vasil) contracts. We don't use it directly.
- [Kopu](https://cardanosolutions.github.io/kupo): is a chain-index to query the blockchain using HTTP protocol. We don't use it directly.

## The Contract Monad

The `Contract` is a newtype wrapper around `QueryM`, which is a `ReaderT` on `QueryEnv` over `Aff`:

```hs
newtype Contract (r :: Row Type) (a :: Type)

-- constructor
Contract (QueryMExtended r Aff a)

newtype QueryMExtended (r :: Row Type) (m :: Type -> Type) (a :: Type)

-- constructor
QueryMExtended (ReaderT (QueryEnv r) m a)
```

The `r :: Row Type` defines the type of `extraConfig` field of the underlying given value for `ReaderT`:

```hs
type QueryEnv (r :: Row Type) = { config :: QueryConfig, extraConfig :: Record r, runtime :: QueryRuntime }
```

## The Contract Monad - Example

Whenever we define a `Contract` type signature, we need to pass the extraConfig type or a placeholder and the returning value of the monad:

```hs
type ExtraConfig = { foo :: String }

submitTx :: Contract ExtraConfig TransactionHash
submitTx = do
  cfg <- ask
  logInfo' $ "Foo: " <> show cfg.foo
  txId <- submit ?bsTx
  logInfo' $ "Tx ID: " <> show txId
  pure txId
```

To run, we need to pass a valid config (with extraConfig set):

```hs
main :: Effect Unit
main = launchAff_ do
  txId <- runContract {..., extraConfig = { foo = "bar" } } submitTx
  log $ show txId
```

```hs
launchAff_ :: forall a. Aff a -> Effect Unit
runContract :: forall (r :: Row Type) (a :: Type). ConfigParams r -> Contract r a -> Aff a
```

## Using a Validator

```hs
lock :: forall r. Int -> Contract r TransactionHash
lock amount = do
  validator <- ?toValidator :: Contract r Validator
  let lookups = SL.validator validator
  buildBalanceSignAndSubmitTx lookups ?constraints
```

```hs
newtype Validator = Validator PlutusScript
```

```hs
toValidator :: Either Error Validator
toValidator = wrap <$> ?parseScript
```
> :bulb: Notice `Error` matching `MonadError Error (Contract r)`.

## Plutus Script Parser

```hs
parseScript :: Either Error PlutusScript
parseScript = plutusV2Script
  <$> (lmap (error <<< printTextEnvelopeDecodeError) $ textEnvelopeBytes ?script PlutusScriptV2)
```

```hs
data TextEnvelopeType = PlutusScriptV1 | PlutusScriptV2

textEnvelopeBytes :: String -> TextEnvelopeType -> Either TextEnvelopeDecodeError ByteArray

printTextEnvelopeDecodeError :: TextEnvelopeDecodeError -> String

lmap :: forall f a b c. Bifunctor f => (a -> b) -> f a c -> f b c

plutusV2Script :: ByteArray -> PlutusScript
```

## Plutus Script FFI

```javascript
// Script.js
exports.script = require("Scripts/always-succeeds-v2.plutus");
```

> :warning: Your loader must know how to `require` a plutus file

```hs
-- Script.purs
foreign import script :: String
```

## Breakthrough

> User Story: As a donator I want to give any amount of ADAs to the first visitor

### Scenario

1. Donator integrates his wallet
2. Donator locks `n` ADA into `Script Address`
3. Visitor see avaliable `n` ADA 
4. Visitor integrates his wallet
5. Visitor grabs all ADA

### Bootstrap

```bash
cd modules/M08-bring-it-on
nix flake init -t github:LovelaceAcademy/nix-templates#pix-ctl-full
git init
git add --all
nix develop
npm install
git add --all
git commit -m "Initial commit"
```

### CLI wallet on Preview with covering funds

- Create `donator.{skey,vkey,addr}` with given `cardano-cli`
- Create `visitor.{skey,vkey,addr}` with given `cardano-cli`
- Send test ADA to donator and visitor address [Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet)
