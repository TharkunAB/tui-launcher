{ pkgs ? import <nixpkgs> { } }:

let
  python = pkgs.python3.withPackages (ps: [ ps.pillow ]);
in
pkgs.mkShell {
  packages = [
    pkgs.haskell.compiler.ghc9122
    pkgs.cabal-install
    pkgs.gnumake
    pkgs.haskellPackages.fourmolu
    pkgs.haskellPackages.cabal-gild
    pkgs.haskellPackages.hlint
    python
  ];
}
