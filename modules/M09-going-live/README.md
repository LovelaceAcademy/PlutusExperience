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

[![Module Video](https://img.youtube.com/vi/q9wvYz1s9-U/0.jpg)](https://www.youtube.com/watch?v=q9wvYz1s9-U&list=PLHJ1yaDcSSafyxLNX1xAE-F8wtlbyKhTT)

## What you should know

1. Typeclasses (module 4)
2. Building smart contracts (module 6)
3. Cardano-transaction-lib (module 8)

# Untyped and Typed Validators

## Recap: The Validator

```haskell
import PlutusTx.Prelude (BuiltinData)
-- ...
validator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
validator datum redeemer context = ()

validator' :: Validator
validator' = mkValidatorScript $$(compile [||validator||])
-- ...
```

> :bulb: Because `PlutusTx.compile` function relies on [TemplateHaskell](https://wiki.haskell.org/Template_Haskell), every definition that needs to be used inside it (on-chain code in our case) needs to have an [inlinable pragma](https://wiki.haskell.org/Inlining_and_Specialisation#What_does_the_INLINABLE_pragma_do.3F) `{-# INLINABLE definition -}`. `PlutusTx.Prelude` exposes definitions with this pragma, in this case, `BuiltinData`.

## What if

```haskell
-- ...
data Password = Password Integer

validator :: BuiltinData -> Password -> BuiltinData -> ()
validator _ (Password n) _ | n == 42 = ()
validator _ _ _ = _shouldFail

validator' :: Validator
validator' = mkValidatorScript $$(compile [||validator||])
-- ...
```


```
Couldn't match type `Password` with `BuiltinData`
Expected type: template-haskell-2.16.0.0:Language.Haskell.TH.Syntax.Q
                 (template-haskell-2.16.0.0:Language.Haskell.TH.Syntax.TExp
                    (PlutusTx.Code.CompiledCode
                       (BuiltinData -> BuiltinData -> BuiltinData -> ())))
  Actual type: th-compat-0.1.4:Language.Haskell.TH.Syntax.Compat.SpliceQ
                 (PlutusTx.Code.CompiledCode
                    (BuiltinData -> Password -> BuiltinData -> ()))
In the expression: compile [|| validator ||]
In the Template Haskell splice $$(compile [|| validator ||])
In the first argument of `mkValidatorScript`, namely
  `$$(compile [|| validator ||])`
```

```
Found hole: _shouldFail :: ()
Or perhaps `_shouldFail` is mis-spelled, or not in scope
In the expression: _shouldFail
In an equation for `validator`: validator _ _ _ = _shouldFail
```

- How to fix the type error?
- How to tell to plutus core to fail?

## Errors

The plutus runtime only accepts one result from the validator: `()` (which is the same as `unit` in PureScript). This is hard-coded as you can see in the expected type. When `()` is returned, it means script has been evaluated, when something wrong happens, anything, the script should throw an exception.

It means that we can use something like:

```haskell
import PlutusTx.Prelude (BuiltinData, error)

-- error :: forall a.  () -> a

validator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
validator _ _ _ = error ()
```


`a` is also know as [`undefined`](https://wiki.haskell.org/Undefined), something that can't be evaluated, trying to evaluate it at runtime will throw an exception.


## BuiltinData

```haskell
data Data =
     Constr Integer [Data]
   | Map [(Data, Data)]
   | List [Data]
   | I Integer
   | B BS.ByteString
   deriving stock (Show, Eq, Ord, Generic)
   deriving anyclass (NFData)

data BuiltinData = BuiltinData Data
```

- `PlutusTx.Data` is a Plutus Core version of a "JSON", with its type constructors you can build a tree of different nested `Data` values. Plutus core runtime only knows about `Data` values. Datum, redeemer and script context values are all `Data` values.
- `BuiltinData`is a INLINABLE `Data` wrapper for on-chain data.

More on [Data type](https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week2.html#plutustx-data)

## mkUntypedValidator

We can build a validator function that uses any type using `mkUntypedValidator`.

```haskell
import Plutus.Script.Utils.Typed (IsScriptContext (mkUntypedValidator))

validator' :: Validator
validator' = mkValidatorScript $$(compile [|| wrap ||])
  where
    wrap = mkUntypedValidator _validator
```

```haskell
class UnsafeFromData sc => IsScriptContext sc where
  {-# INLINABLE mkUntypedValidator #-}
  mkUntypedValidator
        :: (UnsafeFromData d, UnsafeFromData r)
        => (d -> r -> sc -> Bool)
        -> UntypedValidator
```

`d`, `r` and `sc` represents the datum, redeemer and script context types. `Bool` is hardcoded, so if the validator evaluates it should return `True`, `False` otherwise.

We have a default `ScriptContext` that fullfill the sc hole, available in `Plutus.V1.Ledger.Api` and `Plutus.V2.Ledger.Api`.

## mkUntypedValidator in use

```haskell
import Plutus.V2.Ledger.Api (Validator, ScriptContext, mkValidatorScript)
import Plutus.Script.Utils.Typed (IsScriptContext (mkUntypedValidator))

newtype Password = Password Integer

validator :: BuiltinData -> Password -> ScriptContext -> Bool 
validator _ (Password n) _ | n == 42 = True 
validator _ _ _ = error ()

validator' :: Validator
validator' = mkValidatorScript $$(compile [|| wrap ||])
  where
    wrap = mkUntypedValidator validator
```

```
No instance for (PlutusTx.IsData.Class.UnsafeFromData Password)
  arising from a use of `mkUntypedValidator`
In the expression: mkUntypedValidator validator
In an equation for `wrap`: wrap = mkUntypedValidator validator
In an equation for `validator'`:
    validator'
      = mkValidatorScript $$(compile [|| wrap ||])
      where
          wrap = mkUntypedValidator validator

   wrap = mkUntypedValidator validator
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

As you can see `BuiltinData` has an instance for `UnsafeFromData`, but not `Password`.

## UnsafeFromData

```haskell
import PlutusTx (UnsafeFromData (unsafeFromBuiltinData))
import PlutusTx.Builtins.Internal (BuiltinData (BuiltinData), unsafeDataAsI)
import Plutus.V2.Ledger.Api (Validator, ScriptContext, mkValidatorScript)
import Plutus.Script.Utils.Typed (IsScriptContext (mkUntypedValidator))

newtype Password = Password Integer

instance UnsafeFromData Password where
  unsafeFromBuiltinData d = Password (unsafeDataAsI d)
  unsafeFromBuiltinData _ = error ()

validator :: BuiltinData -> Password -> ScriptContext -> Bool 
validator _ (Password n) _ | n == 42 = True 
validator _ _ _ = error ()

validator' :: Validator
validator' = mkValidatorScript $$(compile [|| wrap ||])
  where
    wrap = mkUntypedValidator validator
```

> :bulb: Unsafe terminlogy here means that the type conversion can fail unexpectedly, which will trigger a validator error at runtime. We need to be sure what we're passing to validator is correct. You can also use the safer alternative `FromData` which returns a `Maybe a`, but you'll  need more boilerplate. 

## unstableMakeIsData

Is a template haskell function that creates the instance by reading the type signature of the given type.

```haskell
{-# OPTIONS_GHC -ddump-splices #-}

-- ...

newtype Password = Password Integer

unstableMakeIsData ''Password
{-  GHC output:
Splicing declarations
unstableMakeIsData ''Password
====>
instance PlutusTx.IsData.Class.ToData Password where
  {-# INLINABLE PlutusTx.IsData.Class.toBuiltinData #-}
  PlutusTx.IsData.Class.toBuiltinData (Password arg_a9wO)
    = (PlutusTx.Builtins.Internal.mkConstr 0)
...
instance PlutusTx.IsData.Class.FromData Password where
  {-# INLINABLE PlutusTx.IsData.Class.fromBuiltinData #-}
  PlutusTx.IsData.Class.fromBuiltinData d_a9wP
    = let
...
      in
...
instance UnsafeFromData Password where
  {-# INLINABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData d_a9wY
    = let
...
      in
...
-}

validator :: BuiltinData -> Password -> ScriptContext -> Bool 
validator _ (Password n) _ | n == 42 = True 
validator _ _ _ = error ()
```

The optional `dump-splice` flag is being used to print the code generated by `unstableMakeIsData`.

> :bulb: In haskell a preceding ' means "elevates this definition in the kind level" and  '' means "elevates this definition in the template level".

# Breakthrough: Testing with Plutip

## Description 

> As a donator I want to lock 10 ADA in the contract

> Given that I have fulfilled the correct answer, which is 42, as a visitor I want to get all ADA in the contract

## Bootstrap

```bash
mkdir modules/M09-going-live/{contract,dapp}

(
  cd modules/M09-going-live/contract
  nix flake init -t github:LovelaceAcademy/nix-templates#hix-plutus
)

(
  cd modules/M09-going-live/dapp
  nix flake init -t github:LovelaceAcademy/nix-templates#pix-ctl-full
)
```

## Reference links

- [CTL test example](https://github.com/Plutonomicon/cardano-transaction-lib/blob/e3003b91d97ac02504f8b5e23657189b663d797b/examples/ContractTestUtils.purs)
- [singularitynet](https://github.com/mlabs-haskell/singularitynet)
