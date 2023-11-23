{
  inputs = {
    ctl-nix.url = "github:LovelaceAcademy/ctl-nix";
    nixpkgs.follows = "ctl-nix/nixpkgs";
    purs-nix.follows = "ctl-nix/purs-nix";
    utils.url = "github:ursi/flake-utils";
    minting-policy.url = "path:../contract";
  };

  outputs = { self, utils, ... }@inputs:
    let
      # TODO add missing arm to match standard systems
      #  right now purs-nix is only compatible with x86_64-linux
      systems = [ "x86_64-linux" ];
      overlays = with inputs.ctl-nix.inputs.ctl.overlays; [
        # adds: purs
        purescript
        # adds:
        #  arion
        #  plutip-server
        #  ogmios
        #  kupo
        runtime
      ];
    in
    utils.apply-systems
      { inherit inputs systems overlays; }
      ({ system, pkgs, ctl-nix, minting-policy, ... }:
        let
          # Use purs from CTL instead from nixpkgs
          purs = pkgs.easy-ps.purs-0_14_5;
          purs-nix = inputs.purs-nix {
            inherit system;
            overlays = [ ctl-nix ];
          };
          scripts = pkgs.runCommand
            "scripts"
            {
              buildInputs = [ minting-policy ];
            }
            ''
              mkdir -p $out/Scripts
              minting-policy > $out/Scripts/scriptV2.json
            '';
          ps = purs-nix.purs
            {
              purescript = purs;
              # Project dir (src, test)
              dir = ./.;
              # Dependencies
              dependencies =
                with purs-nix.ps-pkgs;
                [
                  cardano-transaction-lib
                  halogen
                  halogen-formless
                  halogen-storybook
                  read
                ];
              # FFI dependencies
              foreign."Scripts".node_modules = scripts;
              # 
            };
          testRuntime = with pkgs; [
            plutip-server
            ogmios
            kupo
          ];
          ps-command = pkgs.writeShellApplication {
            name = "purs-nix";
            runtimeInputs = testRuntime ++ [
              (ps.command { name = "purs-nix-origin"; })
            ];
            text = ''purs-nix-origin "$@"'';
          };
          # TODO move this patch to ctl-nix
          prebuilt = (pkgs.arion.build {
            inherit pkgs;
            modules =
              let
                # add here the Slot and block header where you want to start syncing
                slot = "11213922";
                id = "3e9029c1dff85bad50e2a0b507d39ef4d745d24a9780b3ce5eda7df307815db2";
                ctl-module = pkgs.buildCtlRuntime {
                  kupo.since = "${slot}.${id}";
                };
                ctl-module' = args:
                  let
                    module-ret = ctl-module args;
                    kupo-cmd = module-ret.services.kupo.service.command;
                  in
                  pkgs.lib.attrsets.recursiveUpdate module-ret {
                    # FIXME remove workaround for defer-db-indexes
                    #  Related Plutonomicon/cardano-transaction-lib#1444
                    services.kupo.service.command =
                      pkgs.lib.lists.subtractLists [ "--defer-db-indexes" ] kupo-cmd;
                  };
              in
              [ ctl-module' ];
          });
          concurrent = pkgs.writeShellApplication {
            name = "concurrent";
            runtimeInputs = with pkgs; [
              concurrently
            ];
            text = ''
              concurrently\
                --color "auto"\
                --prefix "[{command}]"\
                --handle-input\
                --restart-tries 10\
                "$@"
            '';
          };
          # TODO move from docker runtime to a nix runtime
          runtime = pkgs.writeShellApplication {
            name = "runtime";
            runtimeInputs = [ pkgs.arion pkgs.docker ];
            text = ''arion --prebuilt-file ${prebuilt.outPath} "$@"'';
          };
          cardano-cli = pkgs.writeShellApplication {
            name = "cardano-cli";
            runtimeInputs = with pkgs; [ docker ];
            text = ''
              docker volume inspect store_node-preview-ipc || _warn "Cardano node volume not found, run \"dev or runtime\" first."
              docker run --rm -it -v "$(pwd)":/data -w /data -v store_node-preview-ipc:/ipc -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket --entrypoint cardano-cli "inputoutput/cardano-node" "$@"
            '';
          };
          purs-watch = pkgs.writeShellApplication {
            name = "purs-watch";
            runtimeInputs = with pkgs; [ entr ps-command ];
            text = ''find {src,test} | entr -s "purs-nix $*"'';
          };
          webpack = pkgs.writeShellApplication {
            name = "webpack";
            runtimeInputs = with pkgs; [ nodejs ];
            text = ''npx webpack "$@"'';
          };
          serve = pkgs.writeShellApplication {
            name = "serve";
            runtimeInputs = with pkgs; [ webpack ];
            text = ''BROWSER_RUNTIME=1 webpack serve --progress --open "$@"'';
          };
          dev = pkgs.writeShellApplication {
            name = "dev";
            runtimeInputs = with pkgs; [
              concurrent
              runtime
              purs-watch
              serve
            ];
            text = ''
              concurrent \
                "purs-watch compile"\
                serve\
                "runtime up"
            '';
          };
          bundle = pkgs.writeShellApplication {
            name = "bundle";
            runtimeInputs = with pkgs; [ webpack ];
            text = ''BROWSER_RUNTIME=1 webpack --mode=production "$@"'';
          };
          docs = pkgs.writeShellApplication {
            name = "docs";
            runtimeInputs = with pkgs; [
              concurrent
              nodejs
              purs-watch
            ];
            text = ''
              concurrent \
              "purs-watch docs" \
              "npx http-server ./generated-docs/html -o"
            '';
          };
          tests = pkgs.writeShellApplication {
            name = "tests";
            text = ''purs-watch test "$@"'';
            runtimeInputs = [ purs-watch ];
          };
          checks = pkgs.runCommand "checks"
            {
              buildInputs = testRuntime;
            } ''${ps.test.run { }}; touch $out'';
        in
        {
          packages.default = ps.output { };

          checks.default = checks;

          devShells.default =
            pkgs.mkShell
              {
                packages = with pkgs; [
                  runtime
                  cardano-cli
                  easy-ps.purescript-language-server
                  purs
                  ps-command
                  purs-watch
                  dev
                  bundle
                  docs
                  tests
                ];
                shellHook = ''
                  alias log_='printf "\033[1;32m%s\033[0m\n" "$@"'
                  alias info_='printf "\033[1;34m[INFO] %s\033[0m\n" "$@"'
                  alias warn_='printf "\033[1;33m[WARN] %s\033[0m\n" "$@"'
                  log_ "Welcome to pix-ctl-full shell."
                  info_ "Available commands: dev, tests, docs, runtime, cardano-cli, purs-nix, bundle."
                  info_ "testnet-magic for preview is 2"
                '';
              };
        });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [
      "https://klarkc.cachix.org?priority=99"
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
      "https://cache.nixos.org"
      "https://hercules-ci.cachix.org"
    ];
    extra-trusted-public-keys = [
      "klarkc.cachix.org-1:R+z+m4Cq0hMgfZ7AQ42WRpGuHJumLLx3k0XhwpNFq9U="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
    ];
  };
}
