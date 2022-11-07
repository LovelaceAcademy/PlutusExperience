---
title: M03 - Our First Web App
author: Walker Leite
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

# Breakthrough

> User Story: As a visitor I want to give my address to receive a NFT

## Scenario

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
