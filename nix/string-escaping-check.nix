{ lib, runCommand }:
let
  rendered = import ../autodocodec-api-usage/test_resources/nix/expression/strings.nix;
  expected = lib.importJSON ../autodocodec-api-usage/test_resources/nix/expression/strings.json;
in
if rendered == expected
then runCommand "string-escaping-check" { } "touch $out"
else throw (builtins.toJSON { inherit rendered expected; })
