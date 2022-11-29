{
  inputs =
    {
      nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      purs-nix.url = "github:purs-nix/purs-nix";
      ps-tools.follows = "purs-nix/ps-tools";
      utils.url = "github:ursi/flake-utils";
    };

  outputs = { self, utils, ... }@inputs:
    utils.apply-systems { inherit inputs; } ({ pkgs, system, ... }:
      let
        ps-tools = inputs.ps-tools.legacyPackages.${system};
        purs-nix = inputs.purs-nix { inherit system; };

        ps =
          purs-nix.purs
            {
              dependencies =
                with purs-nix.ps-pkgs;
                [
                  console
                  effect
                  prelude
                ];

              dir = ./.;
            };
      in
      {
        packages.default = ps.modules.Main.bundle { };

        devShells.default =
          pkgs.mkShell
            {
              packages =
                with pkgs;
                [
                  entr
                  nodejs
                  (ps.command { })
                  ps-tools.for-0_15.purescript-language-server
                  purs-nix.esbuild
                  purs-nix.purescript
                ];

              shellHook =
                ''
                  alias watch="find src | entr -s 'echo bundling; purs-nix bundle'"
                '';
            };
      }
    );
}


