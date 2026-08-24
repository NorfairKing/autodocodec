{ mkDerivation, aeson, autodocodec, base, bytestring, http-api-data
, lib, text, unordered-containers, vector
}:
mkDerivation {
  pname = "autodocodec-http-api-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring http-api-data text
    unordered-containers vector
  ];
  homepage = "https://github.com/NorfairKing/autodocodec#readme";
  description = "Autodocodec interpreters for http-api-data";
  license = lib.licenses.mit;
}
