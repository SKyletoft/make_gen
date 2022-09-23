{
	description = "A makefile generator";

	inputs = {
		nixpkgs.url     = "github:nixos/nixpkgs/nixos-22.05";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils }:
		flake-utils.lib.eachDefaultSystem(system:
			let
				pkgs     = nixpkgs.legacyPackages.${system};
				version  = "0.0.1";
				src      = self;
					custom-haskell = pkgs.ghc.withPackages(pkgs: with pkgs; [
						split
						directory_1_3_7_0
						extra

						hindent
						stylish-haskell
						QuickCheck
					]);
			in rec {
				packages = {
					artemis-unwrapped = pkgs.rustPlatform.buildRustPackage {
						inherit src version;
						pname              = "artemis";
						cargoSha256        = "sha256-ah8IjShmivS6IWL3ku/4/j+WNr/LdUnh1YJnPdaFdcM=";
						cargoLock.lockFile = "${self}/Cargo.lock";
					};

					default = packages.artemis-wrapped;
				};

				devShell = pkgs.mkShell {
					shellHook = ''
						PS1="\e[32;1mnix-flake: \e[34m\w \[\033[00m\]\n↳ "
					'';
					nativeBuildInputs = with pkgs; [
						custom-haskell
					];
				};
			}
		);
}
