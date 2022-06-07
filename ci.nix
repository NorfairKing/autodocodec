let
  sources = import ./nix/sources.nix;

  versions = {
    "nixos-21.05" = sources.nixpkgs-21_05;
    "nixos-21.11" = sources.nixpkgs-21_11;
  };

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
