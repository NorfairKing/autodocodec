final: prev:
with final.haskell.lib;
{

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
      (
        self: super:
          let
            autodocodecPkg = name:
              buildFromSdist (overrideCabal (self.callPackage (../${name}/default.nix) { }) (old: {
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
                # Ugly hack because we can't just add flags to the 'test' invocation.
                # Show test output as we go, instead of all at once afterwards.
                testTarget = (old.testTarget or "") + " --show-details=direct";
              }));

            autodocodecPackages =
              final.lib.genAttrs [
                "autodocodec"
                "autodocodec-api-usage"
                "autodocodec-openapi3"
                "autodocodec-schema"
                "autodocodec-servant-multipart"
                "autodocodec-swagger2"
                "autodocodec-yaml"
              ]
                autodocodecPkg;
          in
          {
            inherit autodocodecPackages;

            autodocodecRelease =
              final.symlinkJoin {
                name = "autodocodec-release";
                paths = final.lib.attrValues self.autodocodecPackages;
              };
          } // autodocodecPackages
      );
  });
}
