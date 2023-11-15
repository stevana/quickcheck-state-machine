let
  # This was the latest commit on Nov 15 2023.
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/28189330044e1fe145c55dc9560474121ae21ad9";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in

pkgs.mkShell {
  packages = with pkgs; [
    haskell.compiler.ghc963
    haskellPackages.cabal-fmt
    stylish-haskell
    zlib.dev
    postgresql
  ];
}
