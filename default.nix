{ compiler ? "ghc8104" }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  drone-convert-dhall = pkgs.haskell.packages.${compiler}.callPackage ./drone-convert-dhall.nix { };

  name = "pr0ger/drone-convert-dhall";
  tag = "latest";
in pkgs.dockerTools.buildLayeredImage {
  inherit name tag;
  contents = [ pkgs.cacert drone-convert-dhall ];

  config = {
    Cmd = [ "${drone-convert-dhall}/bin/drone-convert-dhall" ];
    ExposedPorts = {
      "3000/tcp" = {};
    };
  };
}
