{ mkDerivation, aeson, aeson-pretty, autodocodec, base, bytestring
, containers, lib, mtl, pretty-show, scientific, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "autodocodec-exact";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring containers mtl
    pretty-show scientific text unordered-containers vector
  ];
  homepage = "https://github.com/NorfairKing/autodocodec#readme";
  description = "Exact decoder for autodocodec";
  license = lib.licenses.mit;
}
