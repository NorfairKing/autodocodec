{ mkDerivation, aeson, autodocodec, base, insert-ordered-containers
, lens, lib, mtl, openapi3, scientific, text, unordered-containers
}:
mkDerivation {
  pname = "autodocodec-openapi3";
  version = "0.2.1.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base insert-ordered-containers lens mtl openapi3
    scientific text unordered-containers
  ];
  homepage = "https://github.com/NorfairKing/autodocodec#readme";
  description = "Autodocodec interpreters for openapi3";
  license = lib.licenses.mit;
}
