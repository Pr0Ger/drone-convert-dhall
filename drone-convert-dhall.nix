{ mkDerivation, aeson, base, bytestring, dhall, dhall-yaml
, filepath, hpack, lib, microlens-platform, microlens-th
, QuickCheck, scotty, tasty, tasty-hspec, text, wai-extra
}:
mkDerivation {
  pname = "drone-convert-dhall";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base dhall dhall-yaml microlens-platform microlens-th text
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring dhall-yaml filepath microlens-platform scotty
    text wai-extra
  ];
  testHaskellDepends = [ aeson base QuickCheck tasty tasty-hspec ];
  prePatch = "hpack";
  homepage = "https://github.com/Pr0Ger/drone-convert-dhall#readme";
  description = "Add support for Drone pipelines configuration in dhall";
  license = lib.licenses.mit;

  enableSharedExecutables = false;
  postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
}
