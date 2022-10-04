---
title: 08 - Bring it on
author: Walker Leite
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
    - You should be familiar with [Foreign Function Interface];
    - You should be familiar with [Monad Transformers];
    - You should be familiar with [Halogen];


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
             Plutus Server
```

- [CTL](https://github.com/Plutonomicon/cardano-transaction-lib) is a Purescript library/interface for building smart contract transactions on Cardano.
- [CSL](https://github.com/Emurgo/cardano-serialization-lib) is a [WASM](https://developer.mozilla.org/en-US/docs/WebAssembly) library to serialize and unserialize plutus compatible data. We don't use it directly.
- [Ogmius](https://github.com/CardanoSolutions/ogmios) is a service to query the blockchain using websocket protocol. We don't use it directly.
- [Ogmius Datum Cache](https://github.com/mlabs-haskell/ogmios-datum-cache) is a service to store datum values for V1 (non-vasil) contracts. We don't use it directly.
- [Plutus Server](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/server): when using paramaterized contracts, Haskell is still required to build the validator using its meta-programming capabilities, this server is used for this.

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
