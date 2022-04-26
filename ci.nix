let
  sources = import ./nix/sources.nix;

  versions = {
    "nixos-21.05" = "ad425b5cfb214f6d94c57638e3fc371d5806562c";
    "nixos-21.11" = "5a2e2471e8163da8e6f2c1dfd50ef9063199c08b";
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
