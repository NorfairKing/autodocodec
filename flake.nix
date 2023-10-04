{
  description = "autodocodec";
  nixConfig = {
    extra-substituters = "https://autodocodec.cachix.org";
    extra-trusted-public-keys = "autodocodec.cachix.org-1:UU3l42g+wSr6tzvawO/oDLo+5yC5BJiATnoV4/AViMs=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-23.05";
    nixpkgs-22_11.url = "github:NixOS/nixpkgs?ref=nixos-22.11";
    nixpkgs-22_05.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    nixpkgs-21_11.url = "github:NixOS/nixpkgs?ref=nixos-21.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    horizon-core.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-core";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-22_11
    , nixpkgs-22_05
    , nixpkgs-21_11
    , pre-commit-hooks
    , horizon-core
    , validity
    , safe-coloured-text
    , fast-myers-diff
    , sydtest
    }:
    let
      system = "x86_64-linux";
      overlays = [
        self.overlays.${system}
        (import (validity + "/nix/overlay.nix"))
        (import (safe-coloured-text + "/nix/overlay.nix"))
        (import (fast-myers-diff + "/nix/overlay.nix"))
        (import (sydtest + "/nix/overlay.nix"))
      ];
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system; inherit overlays;
      };
      horizonPkgs = import nixpkgs {
        inherit system;
        overlays = [
          (final: prev: {
            haskellPackages = prev.haskellPackages.override (old: {
              overrides = final.lib.composeExtensions (old.overridesor (_: _: { })) (self: super:
                horizon-core.legacyPackages.${system} // super
              );
            });
          })
        ] ++ overlays;
      };
      pkgs = pkgsFor nixpkgs;
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = pkgs.haskellPackages.autodocodecPackages;
      checks.${system} =
        let
          backwardCompatibilityCheckFor = nixpkgs:
            let pkgs' = pkgsFor nixpkgs;
            in pkgs'.haskellPackages.autodocodecRelease;
          allNixpkgs = {
            inherit
              nixpkgs-22_11
              nixpkgs-21_11
              nixpkgs-22_05;
          };
          backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
        in
        backwardCompatibilityChecks // {
          forwardCompatibility = horizonPkgs.haskellPackages.autodocodecRelease;
          release = pkgs.haskellPackages.autodocodecRelease;
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              hpack.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
              nixpkgs-fmt.excludes = [ ".*/default.nix" ];
              cabal2nix.enable = true;
            };
          };
        };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "autodocodec-shell";
        packages = p: builtins.attrValues p.autodocodecPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          niv
          zlib
          cabal-install
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
