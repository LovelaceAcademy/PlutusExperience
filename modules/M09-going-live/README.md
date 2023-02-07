---
title: M09 - Going Live
author: Walker Leite
---
# Introduction

## Getting Started

In this module we'll see how to use custom types in our validation script, something critical to build useful smart contracts. We'll also create some tests for the validator using Plutip.

To run this presentation type (you will need [nix](https://nixos.org)):

```sh
../../slide README.md
```

### Community Support

- [LovelaceAcademy Discord](https://discord.gg/fWP9eGdfZ8)
- [StackExchange](https://cardano.stackexchange.com/) (:bulb: use the tag lovelace-academy)
- [Plutonomicon Discord](https://discord.gg/gGFdGaUE)

[Module video] - Coming Soon...

## What you should know

1. Typeclasses (module 4)
2. Building smart contracts (module 6)
3. Cardano-transaction-lib (module 8)

# Untyped and Typed Validators

## Recap: The Validator

```haskell
-- ...
validator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
validator datum redeemer context = ()

validator' :: Validator
validator' = mkValidatorScript $$(compile [||validator||])
-- ...
```

## What if


```haskell
-- ...
data Password = Password Int

validator :: Password -> Password -> BuiltinData -> ()
validator datum redeemer _ | datum == redeemer = ()
validator _ _ _ = _errorSignal

validator' :: Validator
validator' = mkValidatorScript $$(compile [||validator||])
-- ...
```

```
Couldn't match type 'Password' with 'BuiltinData'
Expected type: template-haskell-2.16.0.0:Language.Haskell.TH.Syntax.Q
                 (template-haskell-2.16.0.0:Language.Haskell.TH.Syntax.TExp
                    (PlutusTx.Code.CompiledCode
                       (BuiltinData -> BuiltinData -> BuiltinData -> ())))
  Actual type: th-compat-0.1.4:Language.Haskell.TH.Syntax.Compat.SpliceQ
                 (PlutusTx.Code.CompiledCode
                    (Password -> Password -> BuiltinData -> ()))
In the expression: compile [|| validator ||]
In the Template Haskell splice $$(compile [|| validator ||])
In the first argument of 'mkValidatorScript', namely
  '$$(compile [|| validator ||])'
```

# Breakthrough

