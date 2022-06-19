let
  sources = import ./nix/sources.nix;

  versions = {
    "nixos-22_05" = sources.nixpkgs-22_05;
  };

  mkReleaseForVersion = version: nixpkgs:
    let
      p = import ./nix/pkgs.nix {
        inherit sources nixpkgs;
      };

    in
    p.autodocodecRelease.overrideAttrs (old: { name = "autodocodec-release-${version}"; });
in
{
  release = (import ./nix/pkgs.nix { inherit sources; }).autodocodecRelease;
  pre-commit-check = (import ./nix/pre-commit.nix { }).run;
} // builtins.mapAttrs mkReleaseForVersion versions
