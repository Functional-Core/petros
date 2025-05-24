{
  src = ../../packages/petros-test;
  versionFile = "ops/versions/petros-test.nix";

  cabal.meta = {
    category = "Testing";
    synopsis = "Assorted testing tools and utilities.";
  };

  library = {
    enable = true;
    dependencies = [
      "base"
      "deepseq"

      "hspec"
      "hspec-api"

      "QuickCheck"

      "validity"
      "validity-aeson"
      "validity-bytestring"
      "validity-containers"
      "validity-path"
      "validity-scientific"
      "validity-text"
      "validity-time"
      "validity-unordered-containers"
      "validity-uuid"
      "validity-vector"

      "genvalidity"
      "genvalidity-aeson"
      "genvalidity-bytestring"
      "genvalidity-containers"
      "genvalidity-path"
      "genvalidity-scientific"
      "genvalidity-text"
      "genvalidity-time"
      "genvalidity-unordered-containers"
      "genvalidity-uuid"
      "genvalidity-vector"

      "genvalidity-property"
    ];
    component.other-modules = [
    ];
    component.ghc-options = [
      "-Wmissing-safe-haskell-mode"
    ];
  };

  test = {
    enable = true;
    dependencies = [
    ];
  };
}
