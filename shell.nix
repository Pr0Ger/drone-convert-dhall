{ compiler ? "ghc8104"
}:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  pkg = import ./default.nix { inherit compiler; };
in
pkgs.mkShell {
  inputsFrom = [ pkg ];
  src = null;

  buildInputs = with pkgs; [
    pkgs.zlib

    pkgs.haskellPackages.cabal-install
    pkgs.hpack
    pkgs.just
    pkgs.pre-commit
  ];
}
