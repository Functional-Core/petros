{
  description = "A foundational platform for software engineering in Haskell.";

  inputs.hix.url = "github:tek/hix";
  inputs.hix.inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs =
    { hix, ... }:
    hix.lib.flake {
      compiler = "ghc910";

      ghcVersions = [
        "ghc96"
        "ghc98"
        "ghc910"
      ];
      overrides = import ./ops/overrides.nix;

      envs.dev = {
        haskellTools =
          ghc: with ghc; [
            fourmolu
          ];
      };

      cabal = {
        license = "MPL-2.0";
        license-file = "LICENSE";
        author = "James Burton";

        # TODO: Extract these into a shared flake
        # Pass "isLibrary" to decide between O1 or O2 (for apps)
        ghc-options = [
          "-fhide-source-paths"
          "-Wall"
          "-Wcompat"
          "-Widentities"
          "-Wimplicit-prelude"
          "-Wredundant-constraints"
          "-Wmissing-export-lists"
          # "-Wpartial-fields"
          "-Wmissing-deriving-strategies"
          "-Wunused-packages"
          "-Winvalid-haddock"
          "-Wredundant-bang-patterns"
          "-Woperator-whitespace"
          "-Wredundant-strictness-flags"
          "-Wmissing-safe-haskell-mode"
          "-O1"
        ];

        language = "GHC2021";

        # TODO: Extract these into a shared flake
        default-extensions = [
          "NoImplicitPrelude"
        ];

        meta = {
          maintainer = "james@functionalcore.dev";
          category = "Prelude";
          github = "FunctionalCore/petros";
          extra-source-files = [ "readme.md" ];
        };
      };

      packages.petros-core = {
        src = ./packages/petros-core;
        versionFile = "ops/versions/petros-core.nix";

        cabal.meta.synopsis = "Standard library and Prelude replacement.";

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
          ];
        };

        test = {
          # TODDO
          enable = false;
          dependencies = [
          ];
        };

      };
    };
}

# TODO
# In the future we might want different files for the packages._____ definitions.
