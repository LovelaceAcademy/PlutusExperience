# cowsay.nix
builtins.derivation {
	system = "x86_64-linux";
	name = "cowsay";
	builder = ./build-cowsay.sh;
}
