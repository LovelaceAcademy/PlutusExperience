---
title: M01 - Plutus Emulator
author: Walker Leite
patat:
  eval:
    nix:
      command: sed 's/"/\\"/g' | xargs -0 -I {} bash -c 'nix eval --impure --expr "{}" | nix run nixpkgs#nixfmt'
    bash:
      command: xargs -0 bash -c
---
# Introduction

## Getting Started

In this module we will introduce you to Cardano EUTxO, Nix and we'll play in Plutus Emulator. 

To run this presentation type (you will need [nix](https://nixos.org)):

```sh
../../slide README.md
```

### Community Support

- [LovelaceAcademy Discord](https://discord.gg/fWP9eGdfZ8)
- [StackExchange](https://cardano.stackexchange.com/) (:bulb: use the tag lovelace-academy)
- [Plutonomicon Discord](https://discord.gg/gGFdGaUE)

[![Module Video](https://img.youtube.com/vi/Vw5uCnR6Vzs/0.jpg)](https://www.youtube.com/watch?v=Vw5uCnR6Vzs&list=PLHJ1yaDcSSadb0aO4aBeq3xaoqzXOG9fB)

## What you should know

1. A programming language
    - Although not essential, being familiar with a language will be beneficial in understanding the in-depth content of this course;
    - Knowing JavaScript and HTML will be beneficial;
2. Functional programming paradigm
    - Although not essential, it will help in the understanding of the in-depth content of this course;

## Intended audience

Web developers aiming to build DApps on Cardano.

## Goals

Build your own javascript-backed DAapp using [Plutus](https://developers.cardano.org/docs/smart-contracts/plutus) and [CTL](https://github.com/Plutonomicon/cardano-transaction-lib)

# The EUTxO Model

## Unspent Input

![unspent](images/001.png)

## Transaction

![transaction](images/002.png)

## Spent Input

![spent input](images/003.png)

## Spent Output

![spent output](images/004.png)

## Script Validation

![script validation](images/005.png)

## Script Context

![script context](images/006.png)

## Deep dive

- [Plutus Pionners](https://plutus-pioneer-program.readthedocs.io/en/latest/week1.html)
- [Official Docs](https://developers.cardano.org/docs/get-started/technical-concepts#unspent-transaction-output-utxo)
- [Unofficial Docs](https://docs.cardano.org/learn/eutxo-explainer)

## Nix

### Components

- The package manager `nix-env`
- The registry `nixpkgs`
- The language `default.nix`
- The OS `NixOS`

## Nix language

### Strings and variables

```nix
let thing  = "world"; in ''
    Hello ${thing}!
      This should be 2 spaces away.
  ''
```

## Lists and Sets

```nix
let amount = 1000;
    type = { name = "cake"; };
    flavor = rec {
        name = "lemon";
        related = [ name "pistachio" ];
    };
    product = {
        type = type.name;
        flavor = flavor;
        image = ./lemon-cake.jpg;
        price = 14.99;
    };
in [
    product.flavor.name
    product.image
    product.price
    amount
    product.flavor.related
   ]
```

## Inherit and with

```nix
let 
    attr = { tires = 4; fuel = 100; };
    car = { inherit attr; brand = "ferrari"; };
    grid = { driver = { name = "alonso"; pos = 3; }; };
in {
    inherit car;
    inherit (grid) driver;
    stops = with attr; [
        { inherit fuel; }
        { inherit fuel; inherit tires; }
    ];
}
```

## Comments, globals, functions and imports

```nix
/*
disqualified.nix
builtins is global
*/
driver: builtins.length driver.penalties > 0
```

```nix
let ended = total-laps: race: race.lap >= total-laps;
    race-ended = ended 50;
    # import is also global
    disqualified = import ./disqualified.nix;
    wins = race: driver: race-ended race
                      && !disqualified driver
                      && driver.pos == 1;
    race = { lap = 50; };
in [
    (wins race { pos = 1; penalties = []; })
    (wins race { pos = 2; penalties = [ "false start"]; })
    (wins race { pos = 3; penalties = []; })
   ]
```

## Derivations

Is the result (side-effect) of calling a function that takes a set of attributes, including but not limited to:
- system string (like "x86_64-linux")
- the name string
- builder derivation or path

:bulb: Every attribute is passed as environment variables to the builder, if a derivation attribute is passed, it will be evaluated (aka built) before being passed.

The builder is executed in a given environment where `$out` is set to the path of that derivation.

[Derivation manual](https://nixos.org/manual/nix/stable/language/derivations.html)

## Derivation - Example

```sh
#!/bin/sh
# build-cowsay.sh
echo "moo..." >> $out
```

```
# cowsay.nix
builtins.derivation {
	system = "x86_64-linux";
	name = "cowsay";
	builder = ./build-cowsay.sh;
}
```

```bash
nix-build ./cowsay.nix
```

```bash
cat result
```

## Breakthrough: Plutus Emulator

> Run the default smart contract in the emulator

> Bonus: Run also docs

## Bootstrap

- [Enable IOG Cache](https://github.com/input-output-hk/plutus-apps/#iohk-binary-cache)

```bash
git clone https://github.com/input-output-hk/plutus-apps.git
git checkout 6b8bd0d80115ed5379ada31eea9e4e50db31f126
```

## Links

- [Online Plutus Docs](https://playground.plutus.iohkdev.io/doc)
