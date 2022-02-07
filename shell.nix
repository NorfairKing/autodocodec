let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };
in
pkgs.haskell.lib.buildStackProject {
  name = "autodocodec-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
    zlib
    haskellPackages.doctest
    pkgs.haskell.compiler.ghc921
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook;
}
