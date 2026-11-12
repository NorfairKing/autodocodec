{ lib, runCommand }:
# What an operator gets out of a generated type: the
# options they defined, and an explicit null where they wrote one.
let
  type = import ../autodocodec-api-usage/test_resources/nix/options/unsettable-type.nix { inherit lib; };
  defined = definition:
    (lib.evalModules {
      modules = [
        { options.value = lib.mkOption { inherit type; }; }
        { value = definition; }
      ];
    }).config.value;
  required = {
    required = "r";
    nullable = null;
  };
  withDefaults = required // {
    with-default = "default";
    null-default = null;
  };
  cases = {
    "leaves out the options that were not defined" = {
      definition = required;
      expected = withDefaults;
    };
    "keeps a null that was written" = {
      definition = required // { optional-or-null = null; };
      expected = withDefaults // { optional-or-null = null; };
    };
    "keeps a defined option" = {
      definition = required // { with-default = "other"; };
      expected = required // { with-default = "other"; null-default = null; };
    };
    "leaves out what was not defined in a nested submodule" = {
      definition = required // { nested.one = "1"; };
      expected = withDefaults // { nested.one = "1"; };
    };
    "leaves out what was not defined in each element of a list" = {
      definition = required // { list = [{ one = "1"; } { }]; };
      expected = withDefaults // { list = [{ one = "1"; } { }]; };
    };
    "keeps a null for a nested submodule" = {
      definition = required // { nested-or-null = null; };
      expected = withDefaults // { nested-or-null = null; };
    };
    "leaves out what was not defined in a nullable nested submodule" = {
      definition = required // { nested-or-null.one = "1"; };
      expected = withDefaults // { nested-or-null.one = "1"; };
    };
    "leaves out what was not defined in each value of an attribute set" = {
      definition = required // { map.a.one = "1"; };
      expected = withDefaults // { map.a.one = "1"; };
    };
    "leaves out what was not defined in a submodule in a oneOf" = {
      definition = required // { either.one = "1"; };
      expected = withDefaults // { either.one = "1"; };
    };
    "keeps a value that is not a submodule in a oneOf" = {
      definition = required // { either = "e"; };
      expected = withDefaults // { either = "e"; };
    };
    "leaves out what was not defined in a submodule in a oneOf with a list" = {
      definition = required // { either-list.one = "1"; };
      expected = withDefaults // { either-list.one = "1"; };
    };
    "leaves out what was not defined in each element of a list in a oneOf" = {
      definition = required // { either-list = [{ one = "1"; } { }]; };
      expected = withDefaults // { either-list = [{ one = "1"; } { }]; };
    };
    "keeps a list in a oneOf as it was written when an earlier alternative takes anything" = {
      definition = required // { value-or-list = [{ one = "1"; }]; };
      expected = withDefaults // { value-or-list = [{ one = "1"; }]; };
    };
    "keeps a defined option whose name needs quoting" = {
      definition = required // { "1st" = "1"; };
      expected = withDefaults // { "1st" = "1"; };
    };
    "keeps a recursive value as it was written" = {
      definition = required // { recursive.recurse.recurse = 1; };
      expected = withDefaults // { recursive.recurse.recurse = 1; };
    };
    "leaves out what was not defined in a submodule with a recursive option" = {
      definition = required // { mutually-recursive = { }; };
      expected = withDefaults // { mutually-recursive = { }; };
    };
    "keeps a recursive value in a submodule as it was written" = {
      definition = required // { mutually-recursive.relationshipToA.relationshipToB = { }; };
      expected = withDefaults // { mutually-recursive.relationshipToA.relationshipToB = { }; };
    };
    "merges separate definitions" = {
      definition = lib.mkMerge [ required { nested.one = "1"; } { nested.two = "2"; } ];
      expected = withDefaults // { nested = { one = "1"; two = "2"; }; };
    };
    "leaves out an option whose definition is disabled" = {
      definition = required // { optional = lib.mkIf false "o"; };
      expected = withDefaults;
    };
    "keeps a list of strings in a oneOf with a list of submodules" = {
      definition = required // { strings-or-list = [ "a" ]; };
      expected = withDefaults // { strings-or-list = [ "a" ]; };
    };
    "gives an option that only one alternative of an object has a default for no default" = {
      definition = required // { either-object = { both = "x"; b = "b"; }; };
      expected = withDefaults // { either-object = { both = "x"; b = "b"; }; };
    };
    "keeps a required option of a nested submodule" = {
      definition = required // { nested-required.one = "1"; };
      expected = withDefaults // { nested-required.one = "1"; };
    };
    "leaves out what was not defined in each element of a list of nullable submodules" = {
      definition = required // { list-of-nullable = [ null { one = "1"; } ]; };
      expected = withDefaults // { list-of-nullable = [ null { one = "1"; } ]; };
    };
    "leaves out what was not defined in each element of a list of lists" = {
      definition = required // { list-of-lists = [ [{ one = "1"; } { }] ]; };
      expected = withDefaults // { list-of-lists = [ [{ one = "1"; } { }] ]; };
    };
  };
  # Definitions that leave out an option that has to be defined.
  failingCases = {
    "fails when a required option is not defined" = { nullable = null; };
    "fails when a required nullable option is not defined" = { required = "r"; };
    "fails when the definition of a required option is disabled" = required // { required = lib.mkIf false "r"; };
    "fails when a required option of a defined nested submodule is not defined" = required // { nested-required.two = "2"; };
    "fails when an option that every alternative requires is not defined" = required // { either-object.b = "b"; };
  };
  evaluates = definition: (builtins.tryEval (builtins.deepSeq (defined definition) null)).success;
  failures = lib.filterAttrs (_: c: defined c.definition != c.expected) cases;
  unexpectedSuccesses = builtins.attrNames (lib.filterAttrs (_: evaluates) failingCases);
in
if failures == { } && unexpectedSuccesses == [ ]
then runCommand "unset-options-check" { } "touch $out"
else
  throw (builtins.toJSON {
    inherit unexpectedSuccesses;
    failures = lib.mapAttrs
      (_: c: {
        actual = defined c.definition;
        inherit (c) expected;
      })
      failures;
  })
