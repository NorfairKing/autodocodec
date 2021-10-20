{ sources ? import ./sources.nix
, pkgsf ? sources.nixpkgs
}:
let
  pkgs =
    import pkgsf {
      overlays = [
        (import (sources.validity + "/nix/overlay.nix"))
        (import (sources.safe-coloured-text + "/nix/overlay.nix"))
        (import (sources.sydtest + "/nix/overlay.nix"))
        (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
        (import ./overlay.nix)
      ];
      config.allowUnfree = true;
    };
in
pkgs
