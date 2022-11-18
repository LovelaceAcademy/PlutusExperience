module Main where

import Prelude (($), (<<<))
import Data.Functor ((<$>))
import Data.Map (Map)
import Data.Tuple (Tuple, snd)
import Data.Foldable (and)

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

buildTx :: Inputs -> Outputs -> Redeemer -> Validator -> Boolean
buildTx ins outs redeem validate = and $ (validate redeem outs <<< snd) <$> ins
