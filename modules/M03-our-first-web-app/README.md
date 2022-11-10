---
title: M03 - Our First Web App
author: Walker Leite
patat:
  eval:
    purescript:
      command: purs-eval | node --experimental-network-imports --input-type module
---
# Introduction

## Getting Started

In this module we will introduce you nix flakes and build our first web app using purs-nix.

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

1. PureScript Foundations (module 2)
2. [HTML](https://developer.mozilla.org/en-US/docs/Web/HTML)
3. [Javascript DOM](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model)

# Nix Flakes

## Why

Nix flakes allow us to create nix packages and manage its dependencies.

## Flake Syntax

A `flake.nix` file is just a nix file following some conventions.

> :warning: To use flake you need to have a initialized git repository, otherwise you'll get a "No such file or directory" error

```nix
# flake.nix
{
  inputs = {};
  outputs = {};
}
```

- Inputs: Dependencies of the current package
- Outputs: What the current package generate as output

## Flake commands

You can generate an initial `flake.nix` with the command:

```bash
nix flake init
```

```nix
# flake.nix
{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;

    packages.x86_64-linux.default = self.packages.x86_64-linux.hello;

  };
}
```

> :bulb:  Notice each system output, nix is multi-platform

It will update and lock the input deps (in this case nixpkgs by default) in a `flake.lock`:

```bash
nix flake update
```

To show each output:

```bash
nix flake show
```

To run the package for your system:

```bash
nix run
```

## Purs-nix

You can also pass -t to use some other template:

```bash
nix flake init -t github:purs-nix/purs-nix
```

```nix
#flake.nix
{ inputs =
    { nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      ps-tools.follows = "purs-nix/ps-tools";
      purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
      utils.url = "github:numtide/flake-utils";
    };

  outputs = { nixpkgs, utils, ... }@inputs:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
         let
           #...
         in
         { packages.default = ps.modules.Main.bundle {};

           devShells.default =
             pkgs.mkShell
             # ...
         }
      );
}
```

The `devShells.default` output is the shell used when we run `nix develop`, any derivation being passed in `packages` attribute will be available in the `PATH`.

## purs-watch

One could replace `watch` alias by a derivation:

```nix
#flake.nix
{
  #...
  outputs = { nixpkgs, ...}:
  #...
  let
    pkgs = nixpkgs.legacyPackages.${system};
    #...
    purs-watch = pkgs.writeShellApplication {
      name = "purs-watch";
      runtimeInputs = with pkgs; [ entr ps-command ];
      text = "find src | entr -s 'echo building && purs-nix bundle'";
    };
  in
  {
    devShells.default =
      pkgs.mkShell
        {
          packages =
            with pkgs;
            [
              #...
              purs-watch
            ]
        };
  }
}

```

Finally PureScript can be compiled to JavaScript running `purs-nix` inside of a `nix develop` shell or with `nix build` command.

## Reference links

- [Nix Flakes Wiki](https://nixos.wiki/wiki/Flakes)
- [Purs-nix](https://github.com/purs-nix/purs-nix)

# PureScript

## Infix operators

```purescript
module Main where

import Prelude
import Data.Function (flip)
import Effect.Console (log)
import Test.Assert (assert')

data Roshambo = Rock | Paper | Scissors

beats :: Roshambo -> Roshambo -> Boolean
beats Scissors Paper = true
beats Paper Rock = true
beats Rock Scissors = true
beats _ _ = false

beats' :: Roshambo -> Roshambo -> Boolean
beats' = flip beats

infix 5 beats as !>
infix 5 beats' as <!


main = do
  assert' "scissors should win paper" $ Scissors `beats` Paper
  assert' "paper should lost to scissors" $ not $ Paper !> Scissors
  assert' "rev: scissors should win paper" $ Paper <! Scissors
  assert' "rev: paper should lost to scissors" $ not $ (<!) Scissors Paper
  log "success"
```

## Anonymous Functions

```purescript
module Main where

import Prelude
import Effect.Console (log)
import Test.Assert (assert')

main =do
  assert' "should equal 100" $ 100 == (\v -> v + 90) 10
  assert' "should equal 5" $ 5 == ( (\a -> \b -> a + b) 2 ) 3
  assert' "should equal 5" $ 5 == (\a -> \b -> a + b) 2 3
  assert' "should equal 5" $ 5 == (\a b -> a + b) 2 3
  assert' "should equal 300" $ 300 == ((*) 10) 30
  assert' "should equal 300" $ 300 == ((*)) 10 30
  log "success"
```

## Let in

## Case of

## Function composition

# Breakthrough

## User Story

> As a visitor I want to give my address to receive a NFT

### Scenario

1. Visitor fulfill its address in the form
2. Visitor submit
3. Visitor receives success message

## Bootstrap

```bash
cd modules/M03-our-first-web-app
nix flake init -t github:purs-nix/purs-nix
git init
git add --all
nix develop
git commit -m "Initial commit"
```
