---
title: M06 - Building Smart Contracts
author: Walker Leite
---
# Introduction

## Getting Started

In this module we'll check the Plutus Validator and we'll compile our first smart contract in Haskell.

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

1. Nix (module 1)
2. Nix flakes (module 3)
3. PureScript (modules 2-5)

# Nix Overlays

## Example

Overlay is a feature of nixpkgs (and some flake packages) to allow you to add/override set attributes:

```nix
let pkgs = import <nixpkgs> {
    overlays = [
        (self: super: {
          boost = super.boost.override {
            python = self.python3;
          };
        })
    ];
};
in
    # pkgs.boost with python3
    pkgs.boost;
```

`apply-systems` also receive an `overlays` attribute that apply overlays in the given inputs:


```nix
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.utils.url = "github:ursi/flake-utils";
  outputs = { self, utils, ... }@inputs:
    utils.apply-systems
      {
        inherit inputs;
        overlays = [
            (self: super: {
              ls = super.sl;
            })
        ];
      }
      ({ pkgs, ... }: {
        # Try `ls` in the `nix develop` shell ;-)
        devShells.default = pkgs.mkShell {
            buildInputs = [ pkgs.ls ];
        };
      });
}
```

[More on overlays](https://nixos.org/manual/nixpkgs/stable/#chap-overlays)

# Haskell

## Differences between Haskell and PureScript

In Haskell (GHC) we need to use extensions to enable some builtin PureScript features.

Eg: To create custom kinds we need some extensions

```purescript
-- PureScript
data Token

-- it means Ada type has kind Token
foreign import data Ada :: Token 

data Value :: forall k. k -> Type -> Type
data Value a b = Value b

value1 :: Value Ada Int
value1 = Value 5
```

```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Main where

data Token = Ada

type Value :: forall k. k -> * -> *
data Value a b = MkValue b

value1 :: Value Ada Int
value1 = MkValue 5
```

There are more differences, but in general the compiler will let you know.

[Differences from Haskell](https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md) describes in one page the differences between the languages.
[GHC](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/table.html) describes in one page Haskell extensions.

# Haskell Nix

## Why Haskell Nix

- [Haskell Nix](https://input-output-hk.github.io/haskell.nix)

## Bootstrap

```bash
mkdir haskell-project
cd haskell-project
git init
nix flake init --template github:LovelaceAcademy/nix-templates#haskell-nix
git status
```

```
...
        new file:   .gitignore
        new file:   flake.lock
        new file:   flake.nix
        new file:   hello.cabal
        new file:   nix/hix.nix
        new file:   src/hello.hs
```

## Source code and cabal

```haskell
-- src/hello.hs
module Main (main) where

main = putStrLn "Hello, World!"
```

```cabal
-- hello.cabal
-- ...
executable hello
  hs-source-dirs: src
  main-is: hello.hs
-- ...
```

[More on Cabal](https://cabal.readthedocs.io/en/stable/cabal-package.html)

## Nix flake

```nix
{
  # ...
  outputs = { self, utils, ... }@inputs:
    utils.apply-systems
      {
        inherit inputs;
        # TODO support additional systems
        #  right now we can't afford to test every other system
        systems = [ "x86_64-linux" "aarch64-linux" ];
        overlays = [ inputs.haskell-nix.overlay ];
      }
      ({ pkgs, system, ... }:
        let
          hixProject = pkgs.haskell-nix.hix.project {
            src = ./.;
            evalSystem = "x86_64-linux";
          };
          flake = hixProject.flake { };
        in
        # Flake definition follows hello.cabal
        flake // {
          legacyPackages = pkgs;
        });
        # ...
}
```

## Hix options

```nix
# nix/hix.nix
{pkgs, ...}: {
  # name = "project-name";
  
  # We use the latest supported and cached version
  # from github:input-output-hk/haskell.nix
  compiler-nix-name = "ghc925";

  # Enable for cross-platform build
  # crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
  #   p.mingwW64
  #   p.ghcjs
  # ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
  #   p.musl64
  # ]);

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  # shell.tools.hlint = "latest";
  # shell.tools.haskell-language-server = "latest";
}
```

## Outputs

```bash
# show all outputs of the hello project
nix flake show --allow-import-from-derivation
```

```
git+file:///path/to/project
├───apps
...
│   └───x86_64-linux
│       └───"hello:exe:hello": app
├───checks
...
│   └───x86_64-linux
├───devShell
...
│   └───x86_64-linux: development environment 'ghc-shell-for-hello'
...
└───packages
...
    └───x86_64-linux
        ├───"hello:exe:hello": package 'hello-exe-hello-1.0.0.2'
...
```

## Build and Run

```bash
nix build .#hello:exe:hello
tree result
```

```
result
└── bin
    └── hello
```

```bash
./result/bin/hello
nix run .#hello:exe:hello
```

```
Hello, World!
Hello, World!
```

# Adding Plutus

## Bootstrap

```bash
mkdir plutus-project
cd plutus-project
git init
nix flake init --template github:LovelaceAcademy/nix-templates#plutus
git status
```

```
...
        new file:   .gitignore
        new file:   cabal.project
        new file:   flake.lock
        new file:   flake.nix
        new file:   hello.cabal
        new file:   nix/hix.nix
        new file:   nix/serve-docs.nix
        new file:   src/hello.hs
```

## flake.nix

```nix
{
  # ...
  inputs.iohk-nix.url = "github:input-output-hk/iohk-nix";
  inputs.CHaP.url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
  inputs.CHaP.flake = false;
  # to generate docs
  inputs.plutus.url = "github:input-output-hk/plutus";
  # ...
  outputs = { self, utils, ... }@inputs:
    utils.apply-systems
      {
        # ...
        overlays = [
          inputs.haskell-nix.overlay
          # plutus runtime dependency
          inputs.iohk-nix.overlays.crypto
        ];
      }
      ({ pkgs, system, ... }@context:
        let
          hixProject = pkgs.haskell-nix.hix.project {
            # ...
            inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP; };
            modules = [
              (_: {
                # See input-output-hk/iohk-nix#488
                packages.cardano-crypto-praos.components.library.pkgconfig =
                  pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
                packages.cardano-crypto-class.components.library.pkgconfig =
                  pkgs.lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
              })
            ];
          };
          hixFlake = hixProject.flake { };
          serve-docs = import ./nix/serve-docs.nix inputs context {
            inherit hixProject;
            additionalPkgs = [ "cardano-api" ];
          };
        in
        # ...
}
```

## hello.cabal and cabal.project

```cabal
-- cabal.project
-- ...
repository cardano-haskell-packages
  url: https://input-output-hk.github.io/cardano-haskell-packages
  secure: True
  root-keys:
    3e0cce471cf09815f930210f7827266fd09045445...
-- ...
packages: ./.

package *
  -- Reduce build times, set True for production
  optimization: False
-- ...
```

```cabal
-- hello.cabal
-- ...
executable hello
  import: lang
  hs-source-dirs: src
  main-is: hello.hs
  build-depends:
      base
    , bytestring
    , cardano-api
    , plutus-ledger-api
    , plutus-tx
    , plutus-tx-plugin
    , serialise
```

## hello.hs

```haskell
-- ...
validator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
validator datum redeemer context = ()

validator' :: Validator
validator' = mkValidatorScript $$(compile [||validator||])

serialise :: Validator -> PlutusScript PlutusScriptV2
serialise val = PlutusScriptSerialised $ DBS.toShort $ DBL.toStrict $ CS.serialise $ val

main :: IO ()
main = DBL.putStr $ textEnvelopeToJSON Nothing $ serialise validator'
```

# Breakthrough

## Exercise 

 Use the compiled plutus core in a Cardano preview net transaction
