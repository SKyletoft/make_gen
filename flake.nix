{
	description = "A makefile generator";

	inputs = {
		nixpkgs.url     = "github:nixos/nixpkgs/nixos-22.05";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils }:
		flake-utils.lib.eachDefaultSystem(system:
			let
				pkgs    = nixpkgs.legacyPackages.${system};
				version = "0.0.1";
				src     = self;
				
				deps = with pkgs.haskellPackages; [
					directory_1_3_7_0
					extra
				];
				tools = with pkgs.haskellPackages; [
					hindent
					stylish-haskell
					QuickCheck
				];

				build-haskell = pkgs.ghc.withPackages(pkgs: deps);
				dev-haskell   = pkgs.ghc.withPackages(pkgs: deps ++ tools);
			in rec {
				packages = {
					makegen = pkgs.stdenv.mkDerivation {
						inherit version src;
						pname = "makegen";
						nativeBuildInputs = [ build-haskell ];
					};
					default = packages.makegen;
				};

				devShell = pkgs.mkShell {
					shellHook = ''
						PS1="\e[32;1mnix-flake: \e[34m\w \[\033[00m\]\n↳ "
					'';
					nativeBuildInputs = with pkgs; [ dev-haskell ];
				};
			}
		);
}
