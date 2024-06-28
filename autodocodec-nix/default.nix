{ mkDerivation, autodocodec, base, containers, lib, text
, unordered-containers
}:
mkDerivation {
  pname = "autodocodec-nix";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    autodocodec base containers text unordered-containers
  ];
  homepage = "https://github.com/NorfairKing/autodocodec#readme";
  description = "Autodocodec interpreters for nix";
  license = lib.licenses.mit;
}
