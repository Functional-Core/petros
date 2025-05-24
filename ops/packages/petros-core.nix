{
  src = ../../packages/petros-core;
  versionFile = "ops/versions/petros-core.nix";

  cabal.meta = {
    category = "Prelude";
    synopsis = "Standard library and Prelude replacement.";
  };

  library = {
    enable = true;
    dependencies = [
      "base"
      "bytestring"
      "text"
      "containers"
      "unordered-containers"
      "hashable"
      "deepseq"
      "unliftio"
      "time"
      "semirings"
    ];
    component.other-modules = [
      "Petros.Internal.Basics"
    ];
    component.ghc-options = [
      "-Wmissing-safe-haskell-mode"
    ];
  };

  test = {
    enable = true;
    dependencies = [
      "hspec"
      "hspec-api"
      "hspec-discover"
      "validity"
      "genvalidity"
      "genvalidity-property"
      "QuickCheck"
    ];
  };

  benchmark = {
    enable = true;
    dependencies = [
      "criterion"
    ];
  };
}
