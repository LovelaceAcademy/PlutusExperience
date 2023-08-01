inputs: context:
let
  inherit (context) pkgs;
  inherit (pkgs) lib;
  pickHsPkgs = attrs: hsPkgs: builtins.intersectAttrs
    (
      lib.attrsets.genAttrs attrs (_: true)
    )
    hsPkgs;

in
{ hixProject, additionalPkgs ? [ ] }:
let
  b = builtins;
  inherit (context) system;
  inherit (pkgs.haskell-nix) haskellLib;
  inherit (hixProject) hsPkgs;
  # TODO find a way to get plutus packages from plutus input to deduplicate logic
  plutus-pkgs = [
    "plutus-benchmark"
    "plutus-conformance"
    "plutus-core"
    "plutus-errors"
    "plutus-ledger-api"
    "plutus-metatheory"
    "plutus-tx"
    # FIXME plutus-tx-plugin doc is not building
    # "plutus-tx-plugin"
    # FIXME plutus-script-utils doc is not building
    # "plutus-script-utils"
    "prettyprinter-configurable"
    "word-array"
  ];
  plutus-hsPkgs = pickHsPkgs plutus-pkgs hsPkgs;
  local-hsPkgs = haskellLib.selectProjectPackages hsPkgs;
  extra-hsPkgs = pickHsPkgs additionalPkgs hsPkgs;
  hsPkgs' = plutus-hsPkgs // local-hsPkgs // extra-hsPkgs;
  toHaddock = haskellLib.collectComponents' "library" hsPkgs';
  combine-haddock = inputs.plutus.${system}.plutus.library.combine-haddock
    {
      inherit (pkgs) ghc;
      hspkgs = b.attrValues toHaddock;
      prologue = pkgs.writeTextFile {
        name = "prologue";
        text = "Combined documentation for the project and plutus dependencies";
      };
    };

in
pkgs.writeShellApplication {
  name = "serve-docs";
  runtimeInputs = [
    # TODO move from http-serve to a standalone service to improve performance
    pkgs.nodejs
    combine-haddock
  ];
  text = "npx http-serve ${combine-haddock}/share/doc -o";
}
