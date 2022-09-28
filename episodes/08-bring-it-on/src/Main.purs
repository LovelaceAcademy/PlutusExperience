module Main (main) where

import Contract.Prelude

import Contract.Config as Contract.Config
import Contract.Monad as Contract.Monad
import Scaffold as Scaffold
import Examples.KeyWallet.Internal.Pkh2PkhContract (runKeyWalletContract_)


main :: Effect Unit
main = runKeyWalletContract_ \pkh lovelace unlock -> Scaffold.contract
