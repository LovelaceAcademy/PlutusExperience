{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    purs-eval.url = github:klarkc/purs-eval;
    patat.url = github:klarkc/patat;
    utils.url = github:ursi/flake-utils;
  };

  outputs = { self, utils, ... }@inputs:
    utils.apply-systems
      {
        inherit inputs;
        # limited by purs-nix
        systems = [ "x86_64-linux" ];
      }
      ({ pkgs, purs-eval, patat, ... }: {
        packages.default = pkgs.writeShellApplication {
          name = "slide";
          text = ''
            patat "$@"
          '';
          runtimeInputs = with pkgs; [
            purs-eval
            patat
            nodejs
            haskellPackages.ghc
            w3m
            nodePackages.prettier
          ];
        };
      });

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://cache.iog.io"
      "https://klarkc.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "klarkc.cachix.org-1:R+z+m4Cq0hMgfZ7AQ42WRpGuHJumLLx3k0XhwpNFq9U="
    ];
    allow-import-from-derivation = "true";
  };
}
