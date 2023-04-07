---
title: M11 - A Vesting Contract II
author: Walker Leite
---
# Introduction

## Getting Started

In this module we'll build the vesting contract UI

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

- Vesting contract contract (module 10)
- Halogen (modules 7 and 8);
- Although not required, [variants](https://github.com/natefaubion/purescript-variant) will be useful for [halogen-formless](https://github.com/thomashoneyman/purescript-halogen-formless).

# Breakthrough: Building the Vesting Contract UI

## Description

> As a donator I want to lock an ADA value in a contract, to be rewarded to a given beneficiary according a given deadline

> As the beneficiary I want to reclaim the locked ADA value only after the deadline

## Bootstrap

```bash
cp -Rf modules/M10-a-vesting-contract-i modules/M11-a-vesting-contract-ii
```
