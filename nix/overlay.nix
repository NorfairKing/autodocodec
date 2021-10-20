final:
previous:
with final.haskell.lib;
{
  yamlparseApplicativePackages =
    let
      yamlparseApplicativePkg = name:
        doBenchmark (buildStrictly (final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }));
    in
    final.lib.genAttrs [
      "yamlparse-applicative"
      "yamlparse-applicative-demo"
    ]
      yamlparseApplicativePkg;

  yamlparseApplicativeRelease =
    final.symlinkJoin {
      name = "yamlparse-applicative-release";
      paths = final.lib.attrValues final.yamlparseApplicativePackages;
    };

  haskellPackages = previous.haskellPackages.override (
    old: {
      overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
        self: super: final.yamlparseApplicativePackages
      );
    }
  );
}
