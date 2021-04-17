{ mkDerivation, aeson, base, dhall, dhall-yaml, hpack, lib
, QuickCheck, scotty, tasty, tasty-hspec, text, wai-extra
}:
mkDerivation {
  pname = "drone-convert-dhall";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base dhall dhall-yaml text ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base dhall-yaml scotty text wai-extra
  ];
  testHaskellDepends = [ aeson base QuickCheck tasty tasty-hspec ];
  prePatch = "hpack";
  homepage = "https://github.com/Pr0Ger/drone-convert-dhall#readme";
  description = "Add support for Drone pipelines configuration in dhall";
  license = lib.licenses.mit;

  enableSharedExecutables = false;
  postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
}
