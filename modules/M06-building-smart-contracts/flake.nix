{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.utils.url = "github:ursi/flake-utils";
  outputs = { self, utils, ... }@inputs:
    utils.apply-systems
      {
        inherit inputs;
        overlays = [
            (self: super: {
              ls = super.sl;
            })
        ];
      }
      ({ pkgs, ... }: {
        devShells.default = pkgs.mkShell {
            buildInputs = [ pkgs.sl ];
        };
      });
}
