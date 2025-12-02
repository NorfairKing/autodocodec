{
  description = "autodocodec";
  nixConfig = {
    extra-substituters = "https://autodocodec.cachix.org";
    extra-trusted-public-keys = "autodocodec.cachix.org-1:UU3l42g+wSr6tzvawO/oDLo+5yC5BJiATnoV4/AViMs=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.11";
    nixpkgs-25_05.url = "github:NixOS/nixpkgs?ref=nixos-25.05";
    nixpkgs-24_11.url = "github:NixOS/nixpkgs?ref=nixos-24.11";
    nixpkgs-24_05.url = "github:NixOS/nixpkgs?ref=nixos-24.05";
    nixpkgs-23_11.url = "github:NixOS/nixpkgs?ref=nixos-23.11";
    horizon-advance.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-advance";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-25_05
    , nixpkgs-24_11
    , nixpkgs-24_05
    , nixpkgs-23_11
    , horizon-advance
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , fast-myers-diff
    , sydtest
    , opt-env-conf
    }:
    let
      system = "x86_64-linux";
      nixpkgsFor = nixpkgs: import nixpkgs { inherit system; config.allowUnfree = true; };
      pkgs = nixpkgsFor nixpkgs;
      allOverrides = pkgs.lib.composeManyExtensions [
        (pkgs.callPackage (fast-myers-diff + "/nix/overrides.nix") { })
        (pkgs.callPackage (safe-coloured-text + "/nix/overrides.nix") { })
        (pkgs.callPackage (sydtest + "/nix/overrides.nix") { })
        (pkgs.callPackage (validity + "/nix/overrides.nix") { })
        (pkgs.callPackage (opt-env-conf + "/nix/overrides.nix") { })
        self.overrides.${system}
      ];
      horizonPkgs = horizon-advance.legacyPackages.${system}.extend allOverrides;
      haskellPackagesFor = nixpkgs: (nixpkgsFor nixpkgs).haskellPackages.extend allOverrides;
      haskellPackages = haskellPackagesFor nixpkgs;
    in
    {
      overrides.${system} = pkgs.callPackage ./nix/overrides.nix { };
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = haskellPackages.autodocodecPackages;
      checks.${system} =
        let
          backwardCompatibilityCheckFor = nixpkgs: (haskellPackagesFor nixpkgs).autodocodecRelease;
          allNixpkgs = {
            inherit
              nixpkgs-25_05
              nixpkgs-24_11
              nixpkgs-24_05
              nixpkgs-23_11;
          };
          backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
        in
        backwardCompatibilityChecks // {
          forwardCompatibility = horizonPkgs.autodocodecRelease;
          release = haskellPackages.autodocodecRelease;
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              hpack.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
              nixpkgs-fmt.excludes = [
                ".*/default.nix"
                "autodocodec-api-usage/test_resources/nix/.*.nix"
              ];
              deadnix.enable = true;
              deadnix.excludes = [
                ".*/default.nix"
                "autodocodec-api-usage/test_resources/nix/.*.nix"
              ];
              cabal2nix.enable = true;
              tagref.enable = true;
            };
          };
        };
      devShells.${system}.default = haskellPackages.shellFor {
        name = "autodocodec-shell";
        packages = p: builtins.attrValues p.autodocodecPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          zlib
          cabal-install
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      nix-ci.cachix = {
        name = "autodocodec";
        public-key = "autodocodec.cachix.org-1:UU3l42g+wSr6tzvawO/oDLo+5yC5BJiATnoV4/AViMs=";
      };
    };
}
