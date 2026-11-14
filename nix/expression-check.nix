{ lib, runCommand }:
# Nix reads each rendered expression as the value next to it.
let
  dir = ../autodocodec-api-usage/test_resources/nix/expression;
  read = name: {
    rendered = import (dir + "/${name}.nix");
    expected = lib.importJSON (dir + "/${name}.json");
  };
  failures = lib.filterAttrs (_: c: c.rendered != c.expected) (lib.genAttrs [ "strings" "expressions" ] read);
in
if failures == { }
then runCommand "expression-check" { } "touch $out"
else throw (builtins.toJSON failures)
