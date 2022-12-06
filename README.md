# Plutus Experience

To run anyting on this repository, you'll need to have:

- [Nix](https://nixos.org)
- [IOG Nix Cache - no strict required, but highly recommended](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html#setting-up-the-binary-cache)
- GNU/Linux (it might work on other systems, not tested)

## [Module 1 - Plutus Emulator](modules/M01-plutus-emulator)

- Introduction
  - Background
  - Program goals
  - Requirements
  - Materials
- The EUTxO Model
- Nix language basics
- Plutus Emulator
  - Simulating a simple contract

## [Module 2 - PureScript Foundations](modules/M02-purescript-foundations)
- Introduction to PureScript
  - Why PureScript
  - Types
  - Functions and Currying
  - Pattern Matching
  - Algebric Data Types
  - Kinds
  - Language Reference
- Try PureScript

## [Module 3 - Our First Web App](modules/M03-our-first-web-app)

- Building a Nix flake for a PureScript project
- Let in, case of and anonymous functions
- Infix Function Application
- Function composition
- Writing the HTML of our Web App

## [Module 4 - We should have a map](modules/M04-we-should-have-a-map)

- Typeclasses
- Forall
- Semigroup and Monoids
- Foldable
- Functors
- UTxO transaction builder

## [Module 5 - Burritos Everywhere](modules/M05-burritos-everywhere)

- Apply and Applicative
- Bind, Do notation
- Monads
- IO/Effect
- Contract monad

## [Module 6 - Building Smart Contracts](modules/M06-building-smart-contracts)

- PureScript FFI
- The Validator
- Building a Nix flake for a Plutus project
- Compiling to Plutus Core

## Module 7 - Jupiter

- Row Types and Records
- Monad Transformers
- Aff
- PureScript Halogen
- Web App to show Wallet Funds

## [Module 8 - Bring it on](modules/M08-bring-it-on)

- Off-chain with Cardano-Transaction-Lib
- Building a web app to test the contract
- Integrating with a Wallet

## Module 9 - Going Live

- Untyped vs Typed Validation Scripts
- Running the contract in the testnet with cardano-cli
- Testing with Plutip

## Module 10 - A Vesting Contract

- Script Contexts
- Datums
- Redeemer
- Handling Time (Slots)
- Building the UI

## Module 11 - High Ground

- Metaprogramming in Haskell
- Parameterized contracts
- Running the parameterized contract in CTL

## Module 12 - (Non/)Fungible Tokens
- Values
- Minting Policy
- NFT's
- Building a NFT Minting website
