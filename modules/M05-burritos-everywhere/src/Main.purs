module Main where

import Prelude
  ( class Functor
  , class Apply
  , class Applicative
  , class Bind
  , class Monad
  , ($)
  , (<<<)
  , (<$>)
  , ap
  , pure
  , discard
  )
import Data.Map (Map)
import Data.Tuple (Tuple, snd)
import Data.Foldable (and)
import Effect (Effect)
import Effect.Console (log)

type Address = String
type Index = Int
type Datum = Int
type TransactionHash = String
type TransactionInput = Tuple Index TransactionHash
type TransactionOutput = Tuple Address Datum

type UtxoMap = Map TransactionInput TransactionOutput

type Inputs = UtxoMap
type Outputs = UtxoMap

type Redeemer = Int

type ScriptContext = Outputs

type Validator = Redeemer -> ScriptContext -> Datum -> Boolean

buildTx :: Inputs -> Outputs -> Redeemer -> Validator -> Contract Boolean
buildTx ins outs redeem validate = pure $ and $ (validate redeem outs <<< snd) <$> ins

data Contract a = Contract a

derive instance Functor Contract

instance Apply Contract where
  apply = ap

instance Applicative Contract where
  pure s = Contract s

instance Bind Contract where
  bind (Contract s) fn = fn s

instance Monad Contract

runContract :: forall a. Contract a -> Effect a
runContract (Contract r) = do
  log "contract done"
  pure r
