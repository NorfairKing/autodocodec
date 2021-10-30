let
  sources = import ./nix/sources.nix;

  versions = { };

  mkReleaseForVersion = version: rev:
    let
      pkgsf = builtins.fetchGit {
        url = "https://github.com/NixOS/nixpkgs";
        inherit rev;
      };
      p = import ./nix/pkgs.nix { inherit sources; inherit pkgsf; };
    in
    p.autodocodecRelease.overrideAttrs (old: { name = "autodocodec-release-${version}"; });
in
{
  release = (import ./nix/pkgs.nix { inherit sources; }).autodocodecRelease;
  pre-commit-check = (import ./nix/pre-commit.nix { }).run;
} // builtins.mapAttrs mkReleaseForVersion versions
