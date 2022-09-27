{
  inputs =
    {
      ctl.url = "github:LovelaceAcademy/cardano-transaction-lib";
      nixpkgs.follows = "ctl/nixpkgs";
      purs-nix.url = "github:lovelaceAcademy/purs-nix";
      utils.url = "github:numtide/flake-utils";
    };

  outputs = { self, nixpkgs, utils, ctl, ... }@inputs:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        let
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
          purs = pkgs.easy-ps.purs-0_14_5;
          ps = purs-nix.purs
            {
              # Use the compiler from CTL
              purescript = purs;
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
          packages = {
            default = ps.modules.Main.output { };
            runtime = pkgs.buildCtlRuntime { };
          };

          apps.default = pkgs.launchCtlRuntime { };

          devShells.default =
            pkgs.mkShell
              {
                packages =
                  with pkgs;
                  [
                    entr
                    nodejs
                    (ps.command { })
                    easy-ps.purescript-language-server
                    purs
                  ];

                shellHook =
                  ''
                    alias watch="find src | entr -s 'echo building && purs-nix compile'"
                  '';
              };
        }
      );
}
