---
title: M12 - (Non/)Fungible Tokens
author: Walker Leite
---
# Introduction

## Getting Started

In this module we'll learn about Fungible and Non Fungible Tokens and we'll build a minting solution

To run this presentation type (you will need [nix](https://nixos.org)):

```sh
../../slide README.md
```

### Community Support

- [LovelaceAcademy Discord](https://discord.gg/fWP9eGdfZ8)
- [StackExchange](https://cardano.stackexchange.com/) (:bulb: use the tag lovelace-academy)
- [Plutonomicon Discord](https://discord.gg/gGFdGaUE)

[![Module Video](https://img.youtube.com/vi/Wvy0y7oSTKg/0.jpg)](https://www.youtube.com/watch?v=Wvy0y7oSTKg&list=PLHJ1yaDcSSadhz_cKRMlDxPTd3EmaD_Rr)

## What you should know

- [A Vesting Contract](modules/M11-a-vesting-contract-ii)
- [Script Context](modules/M10-a-vesting-contract-i)
- [Testing with Plutip](modules/M09-going-live)
- [Off-chain with CTL](modules/M08-bring-it-on)

# Parameterized contracts (Plutus and CTL)

## The problem

We already know Datums and Redeemers:

- Datums are data attached to the transaction that are public in the blockchain (can be inspected) they are normally used to represent "contract state";
- Redeemers are data sent to the validator to decide upon its spending according to the "contract state";

```haskell
validator :: MyDatum -> MyRedeemer -> ScriptContext -> Bool
```

But what if you want to inject information in the contract that should not be public in the blockchain?

## Solution

```haskell
validator :: ContractParams -> MyDatum -> MyRedeemer -> ScriptContext -> Bool
```

`validator` is a function, you can just add as many arguments you need, if the validator has more than 3 arguments we say it's a parameterized contract.

What happens behind the scene is that the contract hash will be computed as soon as we apply all missing parameters.

## Solution - Plutus

```haskell
-- ...
data CorrectAnswer = CorrectAnswer Integer
unstableMakeIsData ''CorrectAnswer

data Password = Password Integer

{-# INLINEABLE validator #-}
validator :: CorrectAnswer -> BuiltinData -> Password -> BuiltinData -> ()
validator answer _ (Password n) _ | n == answer = ()
validator _ _ _ = _shouldFail

validator' :: Validator
validator' = Validator $ fromCompiledCode $$(compile [|| wrap ||])
  where
    wrap = validator . unsafeFromBuiltinData
```

## Solution - CTL

```purescript
-- ... on CTL
import Contract.Scripts (PlutusScript, ApplyArgsError, Validator, applyArgs)
-- ...
data CorrectAnswer = CorrectAnswer BigInt

instance ToData CorrectAnswer where
  toData (CorrectAnswer answer) = Constr zero
    [ toData answer
    ]

-- ..

applyError :: ApplyArgsError -> Error
applyError err = error $ "Failed to apply arguments to the script: " <> show err

-- ..

applyScript :: CorrectAnswer -> PlutusScript -> Either Error PlutusScript
applyScript answer ps = applyError `lmap` (applyArgs ps [toData answer])

validator :: CorrectAnswer -> Either Error Validator
validator answer =  wrap <$> (parseScript >>= applyScript answer)
```

# Values

## The problem

Up so far we've used only `Ada` (`lovelace`) token. What if we need to use our own custom native token?

## Solution

```haskell
newtype Value = Value { getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer) }

newtype CurrencySymbol = CurrencySymbol { unCurrencySymbol :: BuiltinByteString }

newtype TokenName = TokenName { unTokenName :: BuiltinByteString }

newtype AssetClass = AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }
```

The convention is to use the script hash hex that have minted the token as `CurrencySymbol`. `TokenName` is free to be used in any way (normally we use it as the token symbol (eg: Ada) or a name + unique id. In the case of `Ada`, its `CurrencySymbol` is an empty string, given `Ada` is the only asset that have been minted in the genesis block (there is no minting script for Ada).

`Value` is also a monoid, you can append together many different `Value`s to produce different combinations.

# Minting Policy

## Explanation

A minting policy is just a script that validates the mint transaction. The validator knows if it's a mint transaction through `ScriptContext`.

```haskell
data ScriptContext = ScriptContext { scriptContextTxInfo :: TxInfo
                                   , scriptContextPurpose :: ScriptPurpose
                                   }
data ScriptPurpose = Minting CurrencySymbol
                   | Spending TxOutRef
                   | Rewarding StakingCredential
                   | Certifying DCert
```

# NFT's

## Explanation

So far we've seen fungible native tokens (eg: Ada). We also can have non-fungible native tokens (or unique tokens of a max quantity of 1). It works in the same way as in `Value`s, but the quantity is always 1.

[More on Native Tokens & NFT's](https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week5.html)

# Breakthrough: Building a NFT Minting website
