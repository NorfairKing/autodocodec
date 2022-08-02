{ sources ? import ./sources.nix
, nixpkgs ? sources.nixpkgs
, system ? builtins.currentSystem
}:
import nixpkgs {
  overlays = [
    (import (sources.validity + "/nix/overlay.nix"))
    (import (sources.safe-coloured-text + "/nix/overlay.nix"))
    (import (sources.sydtest + "/nix/overlay.nix"))
    (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
    (import ./overlay.nix)
  ];
  config.allowUnfree = true;
}
