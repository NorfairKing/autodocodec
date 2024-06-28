{ mkDerivation, autodocodec, autodocodec-schema, base, lib, text }:
mkDerivation {
  pname = "autodocodec-nix";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    autodocodec autodocodec-schema base text
  ];
  homepage = "https://github.com/NorfairKing/autodocodec#readme";
  description = "Autodocodec interpreters for nix";
  license = lib.licenses.mit;
}
