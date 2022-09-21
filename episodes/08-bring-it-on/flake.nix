{
  inputs =
    {
      ctl.url = "github:LovelaceAcademy/cardano-transaction-lib";
      nixpkgs.follows = "ctl/nixpkgs";
      ps-tools.follows = "purs-nix/ps-tools";
      purs-nix.url = "github:lovelaceAcademy/purs-nix";
      utils.url = "github:numtide/flake-utils";
    };

  outputs = { nixpkgs, utils, ctl, ... }@inputs:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        let
          ps-tools = inputs.ps-tools.legacyPackages.${system};
          purs-nix = inputs.purs-nix { inherit system; };
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              # Required by CTL
              ctl.overlays.purescript
              ctl.overlays.ctl-server
              ctl.overlays.runtime
            ];
          };
          ps =
            purs-nix.purs
              {
                # Use the compiler from CTL
                purescript = pkgs.easy-ps.purescript;
                # Project dir (src, test)
                dir = ./.;
                # Dependencies
                dependencies =
                  with purs-nix.ps-pkgs-ns.lovelaceAcademy;
                  [
                    cardano-transaction-lib
                  ];
                # FFI dependencies
                # foreign.Main.node_modules = [];
              };
        in
        {
          packages.default = ps.modules.Main.app { name = "bring-it-on"; };

          devShells.default =
            pkgs.mkShell
              {
                packages =
                  with pkgs;
                  [
                    entr
                    nodejs
                    (ps.command { })
                    ps-tools.for-0_14.purescript-language-server
                  ];

                shellHook =
                  ''
                    alias watch="find src | entr -s 'echo bundling; purs-nix bundle'"
                  '';
              };
        }
      );
}
