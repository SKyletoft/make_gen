{ pkgs ? import <nixpkgs> {} }:
let
	custom-haskell = pkgs.ghc.withPackages(pkgs: with pkgs; [
		split
		directory_1_3_8_0
		extra

		hindent
		stylish-haskell
		QuickCheck
	]);
in pkgs.mkShell {
	# nativeBuildInputs is usually what you want -- tools you need to run
	nativeBuildInputs = with pkgs; [
		custom-haskell
	];
	shellHook = ''
		PS1="\e[32;1mnix-shell: \e[34m\w \[\033[00m\]\n↳ "
	'';
}

