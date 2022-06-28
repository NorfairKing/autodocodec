{ sources ? import ./nix/sources.nix
, nixpkgs ? sources.nixpkgs
, pkgs ? import ./nix/pkgs.nix { inherit nixpkgs sources; }
, pre-commit ? import ./nix/pre-commit.nix { inherit sources; }
}:
pkgs.haskell.lib.buildStackProject {
  name = "autodocodec-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
    zlib
    haskellPackages.doctest
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook;
}
