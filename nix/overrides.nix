{ lib
, haskell
, symlinkJoin
, ...
}:
with lib;
with haskell.lib;
self: super:
let
  autodocodecPkg = name:
    buildFromSdist (overrideCabal (self.callPackage (../${name}) { }) (old: {
      doBenchmark = true;
      configureFlags = (old.configureFlags or [ ]) ++ [
        # Optimisations
        "--ghc-options=-O2"
        # Extra warnings
        "--ghc-options=-Wall"
        "--ghc-options=-Wincomplete-uni-patterns"
        "--ghc-options=-Wincomplete-record-updates"
        "--ghc-options=-Wpartial-fields"
        "--ghc-options=-Widentities"
        "--ghc-options=-Wredundant-constraints"
        "--ghc-options=-Wcpp-undef"
        "--ghc-options=-Werror"
        "--ghc-options=-Wno-deprecations"
      ];
      # Turn off tests for anything but the current GHC's because different
      # versions make for different outputs sometimes.
      doCheck = self.ghc.version == "9.8.4";
    }));

  autodocodecPackages = {
    autodocodec = autodocodecPkg "autodocodec";
    autodocodec-api-usage = autodocodecPkg "autodocodec-api-usage";
    autodocodec-exact = autodocodecPkg "autodocodec-exact";
    autodocodec-nix = autodocodecPkg "autodocodec-nix";
    autodocodec-openapi3 = autodocodecPkg "autodocodec-openapi3";
    autodocodec-schema = autodocodecPkg "autodocodec-schema";
    autodocodec-servant-multipart = autodocodecPkg "autodocodec-servant-multipart";
    autodocodec-swagger2 = autodocodecPkg "autodocodec-swagger2";
    autodocodec-yaml = autodocodecPkg "autodocodec-yaml";
  };
in
{
  inherit autodocodecPackages;

  autodocodecRelease =
    symlinkJoin {
      name = "autodocodec-release";
      paths = attrValues self.autodocodecPackages;
      passthru = self.autodocodecPackages;
    };

  openapi3 =
    if super.openapi3.meta.broken
    then dontCheck (unmarkBroken super.openapi3)
    else super.openapi3;
} // autodocodecPackages
