{ compiler ? "ghc8104"
}:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
pkgs.haskell.packages.${compiler}.callPackage ./drone-convert-dhall.nix { }
