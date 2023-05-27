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
{ hsPkgs, additionalPkgs ? [ ] }:
let
  b = builtins;
  inherit (context) system;
  # TODO find a way to get plutus packages from plutus input to deduplicate logic
  plutus-pkgs = [
    "plutus-benchmark"
    "plutus-conformance"
    # FIXME plutus-core doc is not building
    #"plutus-core"
    "plutus-errors"
    "plutus-ledger-api"
    "plutus-metatheory"
    "plutus-tx"
    "plutus-tx-plugin"
    # FIXME plutus-script-utils doc is not building
    #"plutus-script-utils"
    "prettyprinter-configurable"
    "word-array"
  ];
  plutus-hsPkgs = pickHsPkgs plutus-pkgs hsPkgs;
  extra-hsPkgs = pickHsPkgs additionalPkgs hsPkgs;
  hsPkgs' = plutus-hsPkgs // extra-hsPkgs;
  combine-haddock = inputs.plutus.${system}.plutus.library.combine-haddock
    {
      inherit (pkgs) ghc;
      hspkgs = b.map
        pkgs.haskell.lib.doHaddock
        (b.attrValues hsPkgs');
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
