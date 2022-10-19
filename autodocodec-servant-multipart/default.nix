{ mkDerivation, aeson, autodocodec, base, bytestring, lib
, servant-multipart, servant-multipart-api, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "autodocodec-servant-multipart";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring servant-multipart
    servant-multipart-api text unordered-containers vector
  ];
  homepage = "https://github.com/NorfairKing/autodocodec#readme";
  description = "Autodocodec interpreters for Servant Multipart";
  license = lib.licenses.mit;
}
