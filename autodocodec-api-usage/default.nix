{ mkDerivation, aeson, autodocodec, autodocodec-exact
, autodocodec-http-api-data, autodocodec-nix, autodocodec-openapi3
, autodocodec-schema, autodocodec-servant-multipart
, autodocodec-swagger2, autodocodec-yaml, base, bytestring
, containers, criterion, deepseq, dlist, genvalidity
, genvalidity-aeson, genvalidity-containers, genvalidity-criterion
, genvalidity-dlist, genvalidity-scientific, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-text, genvalidity-time
, http-api-data, lib, openapi3, pretty-show, QuickCheck
, safe-coloured-text, scientific, servant-multipart
, servant-multipart-api, swagger2, sydtest, sydtest-aeson
, sydtest-discover, text, time, unordered-containers, vector, yaml
}:
mkDerivation {
  pname = "autodocodec-api-usage";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-http-api-data autodocodec-openapi3
    autodocodec-schema autodocodec-servant-multipart
    autodocodec-swagger2 autodocodec-yaml base bytestring deepseq
    genvalidity genvalidity-aeson genvalidity-scientific
    genvalidity-text http-api-data openapi3 QuickCheck scientific
    servant-multipart servant-multipart-api swagger2 text
    unordered-containers yaml
  ];
  testHaskellDepends = [
    aeson autodocodec autodocodec-exact autodocodec-http-api-data
    autodocodec-nix autodocodec-openapi3 autodocodec-schema
    autodocodec-servant-multipart autodocodec-swagger2 autodocodec-yaml
    base bytestring containers dlist genvalidity genvalidity-aeson
    genvalidity-containers genvalidity-dlist genvalidity-scientific
    genvalidity-sydtest genvalidity-sydtest-aeson genvalidity-text
    genvalidity-time http-api-data openapi3 pretty-show QuickCheck
    safe-coloured-text scientific servant-multipart-api swagger2
    sydtest sydtest-aeson text time unordered-containers vector yaml
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    aeson autodocodec base bytestring containers criterion deepseq
    genvalidity-containers genvalidity-criterion genvalidity-sydtest
    genvalidity-text genvalidity-time QuickCheck scientific text time
    vector
  ];
  homepage = "https://github.com/NorfairKing/autodocodec#readme";
  description = "Autodocodec api usage tests";
  license = lib.licenses.mit;
}
