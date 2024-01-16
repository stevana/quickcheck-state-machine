let
  # This was the latest commit on Jan 10 2024.
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/cb919df382f2b0dff6cabd27c3719fff0d4a7423";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in

pkgs.mkShell {
  packages = with pkgs; [
    haskell.compiler.ghc981
    haskellPackages.cabal-fmt
    stylish-haskell
    zlib.dev
    postgresql
  ];
}
