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
                  halogen
                ];
              # FFI dependencies
              # foreign.Main.node_modules = [];
            };
          ps-command = ps.command { };
        in
        {
          packages.default = ps.modules.Main.output { };

          devShells.default =
            let
              prebuilt = (pkgs.arion.build {
                inherit pkgs;
                modules = [ (pkgs.buildCtlRuntime { }) ];
              }).outPath;
              runtime = pkgs.writeShellApplication {
                name = "runtime";
                runtimeInputs = [ pkgs.arion pkgs.docker ];
                text =
                  ''
                    ${pkgs.arion}/bin/arion --prebuilt-file ${prebuilt} "$@"
                  '';
              };
              purs-watch = pkgs.writeShellApplication {
                name = "purs-watch";
                runtimeInputs = with pkgs; [ entr ps-command ];
                text = "find src | entr -s 'echo building && purs-nix compile'";
              };
              cardano-cli = pkgs.writeShellApplication {
                name = "cardano-cli";
                runtimeInputs = with pkgs; [ docker ];

                text = ''
                  docker volume inspect store_node-preprod-ipc || echo "[WARN]: Cardano node volume not found, run \"dev\" first."
                  docker run --rm -it -v "$(pwd)":/data -w /data -v store_node-preprod-ipc:/ipc -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket --entrypoint cardano-cli "inputoutput/cardano-node" "$@"
                '';
              };
            in
            pkgs.mkShell
              {
                packages =
                  with pkgs;
                  [
                    nodejs
                    easy-ps.purescript-language-server
                    purs
                    ps-command
                    purs-watch
                    runtime
                    docker
                    cardano-cli
                  ];
                shellHook = ''
                  alias dev="npm run dev"
                  alias bundle="npm run bundle"
                  echo "[INFO]: testnet-magic for preprod is 1"
                '';
              };
        }
      );
}
