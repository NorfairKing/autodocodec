let
  sources = import ./nix/sources.nix;

  versions = {
    "lts-13_19" = "82d2c663b4dffbd635ed694bcc301284987b8097";
    "lts-14_23" = "a87b506140a7267477103759c3f8da5b2e8d994e";
    "lts-15_03" = "beeb24f1e939be7d85fdd64e31f13b8fe8238150";
    "lts-16_11" = "89db531aea80df58584c9a9e3504ffd9617e6b48";
  };

  mkReleaseForVersion = version: rev:
    let
      pkgsf = builtins.fetchGit {
        url = "https://github.com/NixOS/nixpkgs";
        inherit rev;
      };
      p = import ./nix/pkgs.nix { inherit sources; inherit pkgsf; };
    in
    p.yamlparseApplicativeRelease.overrideAttrs (old: { name = "yamlparse-applicative-release-${version}"; });
in
{
  release = (import ./nix/pkgs.nix { inherit sources; }).yamlparseApplicativeRelease;
  pre-commit-check = (import ./nix/pre-commit.nix { }).run;
} // builtins.mapAttrs mkReleaseForVersion versions
