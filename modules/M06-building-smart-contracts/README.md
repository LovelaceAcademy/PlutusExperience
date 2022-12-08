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

# Haskell Nix

## Why Haskell Nix

- [Haskell Nix](https://input-output-hk.github.io/haskell.nix)

## Bootstrap

```bash
mkdir plutus-project
cd plutus-project
git init
nix flake init --template github:LovelaceAcademy/templates#haskell-nix
git status
```

```
...
        new file:   .gitignore
        new file:   LICENSE
        new file:   Setup.hs
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
  outputs = { self, utils, haskellNix, ... }@inputs:
    utils.apply-systems
      {
        inherit inputs;
        overlays = [ haskellNix.overlay ];
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
  compiler-nix-name = "ghc8107"; # Version of GHC to use

  crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
    p.mingwW64
    p.ghcjs
  ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
    p.musl64
  ]);

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



# Breakthrough

## Exercise 

