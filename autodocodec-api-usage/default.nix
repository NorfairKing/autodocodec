{ mkDerivation, aeson, autodocodec, autodocodec-openapi3
, autodocodec-schema, autodocodec-servant-multipart
, autodocodec-swagger2, autodocodec-yaml, base, bytestring
, containers, criterion, deepseq, genvalidity, genvalidity-aeson
, genvalidity-containers, genvalidity-criterion
, genvalidity-scientific, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-text, genvalidity-time
, lib, openapi3, pretty-show, QuickCheck, safe-coloured-text
, scientific, servant-multipart, servant-multipart-api, swagger2
, sydtest, sydtest-aeson, sydtest-discover, text, time
, unordered-containers, yaml
}:
mkDerivation {
  pname = "autodocodec-api-usage";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-openapi3 autodocodec-schema
    autodocodec-servant-multipart autodocodec-swagger2 autodocodec-yaml
    base bytestring deepseq genvalidity genvalidity-aeson
    genvalidity-scientific genvalidity-text openapi3 QuickCheck
    scientific servant-multipart servant-multipart-api swagger2 text
    unordered-containers yaml
  ];
  testHaskellDepends = [
    aeson autodocodec autodocodec-openapi3 autodocodec-schema
    autodocodec-servant-multipart autodocodec-swagger2 autodocodec-yaml
    base bytestring containers genvalidity genvalidity-aeson
    genvalidity-containers genvalidity-scientific genvalidity-sydtest
    genvalidity-sydtest-aeson genvalidity-text genvalidity-time
    openapi3 pretty-show QuickCheck safe-coloured-text scientific
    servant-multipart-api swagger2 sydtest sydtest-aeson text time yaml
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    aeson autodocodec base bytestring containers criterion deepseq
    genvalidity-containers genvalidity-criterion genvalidity-sydtest
    genvalidity-text genvalidity-time QuickCheck scientific text time
  ];
  homepage = "https://github.com/NorfairKing/autodocodec#readme";
  description = "Autodocodec api usage tests";
  license = lib.licenses.mit;
}
