final:
previous:
with final.haskell.lib;
{
  autodocodecPackages =
    let
      autodocodecPkg = name:
        doBenchmark (buildStrictly (final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }));
    in
    final.lib.genAttrs [
      "autodocodec"
      "autodocodec-aeson"
      "autodocodec-aeson-schema"
      "autodocodec-yaml"
      "autodocodec-api-usage"
    ]
      autodocodecPkg;

  autodocodecRelease =
    final.symlinkJoin {
      name = "autodocodec-release";
      paths = final.lib.attrValues final.autodocodecPackages;
    };

  haskellPackages = previous.haskellPackages.override (
    old: {
      overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
        self: super: final.autodocodecPackages
      );
    }
  );
}
