{ mkDerivation, autodocodec, base, containers, lib, text }:
mkDerivation {
  pname = "autodocodec-nix";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ autodocodec base containers text ];
  homepage = "https://github.com/NorfairKing/autodocodec#readme";
  description = "Autodocodec interpreters for nix";
  license = lib.licenses.mit;
}
