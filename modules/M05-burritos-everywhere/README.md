---
title: M05 - Burritos Everywhere
author: Walker Leite
---
# Introduction

## Getting Started

In this module we will see more fundamental type classes of PureScript and Haskell.

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

1. PureScript (modules 2-4)

# Apply and Applicatives

## Recap Functor

![fmap_just](images/fmap_just.png)

## Recap Functor Type Class

Functor is a type class which enables map operations over some type

```haskell
class Functor :: (Type -> Type) -> Constraint
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

infixl 4 map <$>
```

So we can:

```haskell
(+) 3 <$> Just 2
-- Just 5
```

## Functor problem

What happens when you map a function that receives more than one argument?

```haskell
-- + :: Int -> Int -> Int
x = (+) <$> Just 3
-- ok: x = Just ((+) 3) :: Maybe (Int -> Int)
y = x ?? Just 2
-- ?? can't be <$> :: (a -> b) -> f a -> f b
-- what ?? would be to have: y = Just 5 :: Maybe Int
```

## Applicative Just

![applicative_just](images/applicative_just.png)

## Apply Type Class

```haskell
class Apply :: (Type -> Type) -> Constraint
class (Functor f) <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b

infixl 4 apply <*>
```

So we can:

```haskell
x = (+) <$> Just 2
-- x = Just ((+) 3) :: Maybe (Int -> Int)

y =  x <*> Just 3
-- y = Just 5 :: Maybe Int
```

Or even:

```haskell
(+) <$> Just 2 <*> Just 3
-- Just 5 :: Maybe Int
```

You can have any number of arguments:

```haskell
foo = functionTakingNArguments <$> computationProducingArg1
                               <*> computationProducingArg2
                               <*> ...
                               <*> computationProducingArgN
```

> :bulb: nothing says that computations must happen sequentially, actually `Apply` allows parallel computation and we'll see it in the future

## Apply problem

What if you don't know the box type, eg:

```haskell
x :: forall f. Apply f => f Int
x = (+) <$> ?? 2 <*> ?? 3
-- ?? would be :: Int -> f Int
```

## Applicative Type Class

```haskell
class Applicative :: (Type -> Type) -> Constraint
class (Apply f) <= Applicative f where
  pure :: forall a. a -> f a
```

So we can:


```haskell
x :: forall f. Applicative f => f Int
x = (+) <$> pure 2 <*> pure 3

y :: Maybe Int
y = x
-- like magic: y = Just 5
```

# Breakthrough

## Exercise 

In module 04 we've used `buildTx` to simulate a transaction build, in this module we'll simulate a Contract behavior using what we've learned so far.

```purescript
type Validator = Redeemer -> ScriptContext -> Datum -> Boolean

data Contract a = Contract a

buildTx :: Inputs -> Outputs -> Redeemer -> Validator -> Contract Boolean

runContract :: forall a. Contract a -> Effect a
```

Change previous created `buildTx` to return the value using the `Contract` monad and implement the missing definitions. `runContract` must log in the console the contract execution. 
