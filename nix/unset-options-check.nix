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
  # The values that these definitions give, which the autodocodec-api-usage
  # test suite parses with the codec that the type was generated from.
  expectedValues = lib.importJSON ../autodocodec-api-usage/test_resources/nix/options/unsettable-values.json;
  definitions = {
    "leaves out the options that were not defined" = required;
    "keeps a null that was written" = required // { optional-or-null = null; };
    "keeps a defined option" = required // { with-default = "other"; };
    "leaves out what was not defined in a nested submodule" = required // { nested.one = "1"; };
    "leaves out what was not defined in each element of a list" = required // { list = [{ one = "1"; } { }]; };
    "keeps a null for a nested submodule" = required // { nested-or-null = null; };
    "leaves out what was not defined in a nullable nested submodule" = required // { nested-or-null.one = "1"; };
    "leaves out what was not defined in each value of an attribute set" = required // { map.a.one = "1"; };
    "leaves out what was not defined in a submodule in a oneOf" = required // { either.one = "1"; };
    "keeps a value that is not a submodule in a oneOf" = required // { either = "e"; };
    "leaves out what was not defined in a submodule in a oneOf with a list" = required // { either-list.one = "1"; };
    "leaves out what was not defined in each element of a list in a oneOf" = required // { either-list = [{ one = "1"; } { }]; };
    "keeps a list in a oneOf as it was written when an earlier alternative takes anything" = required // { value-or-list = [{ one = "1"; }]; };
    "keeps a defined option whose name needs quoting" = required // { "1st" = "1"; };
    "keeps a recursive value as it was written" = required // { recursive.recurse.recurse = 1; };
    "leaves out what was not defined in a submodule with a recursive option" = required // { mutually-recursive = { }; };
    "keeps a recursive value in a submodule as it was written" = required // { mutually-recursive.relationshipToA.relationshipToB = { }; };
    "merges separate definitions" = lib.mkMerge [ required { nested.one = "1"; } { nested.two = "2"; } ];
    "leaves out an option whose definition is disabled" = required // { optional = lib.mkIf false "o"; };
    "keeps a list of strings in a oneOf with a list of submodules" = required // { strings-or-list = [ "a" ]; };
    "gives an option that only one alternative of an object has a default for no default" = required // { either-object = { both = "x"; b = "b"; }; };
    "keeps a required option of a nested submodule" = required // { nested-required.one = "1"; };
    "leaves out what was not defined in each element of a list of nullable submodules" = required // { list-of-nullable = [ null { one = "1"; } ]; };
    "keeps a value of the first of two submodules in a oneOf" = required // { either-objects.a = "a"; };
    "keeps a value of the second of two submodules in a oneOf" = required // { either-objects.b = "b"; };
    "keeps a value of a discriminated union" = required // { these = { type = "this"; text = "t"; }; };
    "keeps definitions with a priority" = required // { optional = lib.mkDefault "d"; with-default = lib.mkForce "f"; };
    "leaves out what was not defined in each element of a list of lists" = required // { list-of-lists = [ [{ one = "1"; } { }] ]; };
  };
  # Definitions that leave out an option that has to be defined.
  failingCases = {
    "fails when a required option is not defined" = { nullable = null; };
    "fails when a required nullable option is not defined" = { required = "r"; };
    "fails when the definition of a required option is disabled" = required // { required = lib.mkIf false "r"; };
    "fails when every definition of the submodule is disabled" = lib.mkIf false required;
    "fails when a required option of a defined nested submodule is not defined" = required // { nested-required.two = "2"; };
    "fails when an option that every alternative requires is not defined" = required // { either-object.b = "b"; };
    "fails when the discriminator of a discriminated union is not defined" = required // { these.text = "t"; };
  };
  evaluates = value: (builtins.tryEval (builtins.deepSeq value null)).success;
  values = lib.mapAttrs (_: defined) definitions;
  # Another declaration of the option would make it a plain submodule, whose
  # value is not what was defined, so the module system has to refuse to merge
  # the declarations.
  declaredAgain = modules:
    (lib.evalModules {
      modules = modules ++ [{ value = required; }];
    }).options.value.type.name;
  generatedDeclaration = { options.value = lib.mkOption { inherit type; }; };
  plainDeclaration = {
    options.value = lib.mkOption {
      type = lib.types.submodule { options.extra = lib.mkOption { type = lib.types.str; default = "e"; }; };
    };
  };
  unexpectedSuccesses = builtins.attrNames (lib.filterAttrs (_: evaluates) (
    lib.mapAttrs (_: defined) failingCases // {
      "fails when the option is declared again with a submodule after it" = declaredAgain [ generatedDeclaration plainDeclaration ];
      "fails when the option is declared again with a submodule before it" = declaredAgain [ plainDeclaration generatedDeclaration ];
      "fails when the option is declared again with the same type" = declaredAgain [ generatedDeclaration generatedDeclaration ];
    }
  ));
  # The documentation would otherwise give the empty value of the type of an
  # option without a default, like null for a nullOr, as its default.
  documentedDefaults =
    let
      documented = lib.optionAttrSetToDocList (lib.evalModules {
        modules = [{ options.value = lib.mkOption { inherit type; }; }];
      }).options;
      defaultOf = name: (lib.findFirst (o: o.name == name) { } documented).default or null;
    in
    lib.genAttrs (builtins.attrNames expectedDocumentedDefaults) defaultOf;
  expectedDocumentedDefaults = {
    "value.nullable" = lib.literalMD "None, required" // { required = true; };
    "value.optional-or-null" = lib.literalMD "Unspecified";
    "value.list" = lib.literalMD "Unspecified";
    "value.nested" = null;
    "value.nested-or-null" = null;
    "value.with-default" = lib.literalExpression ''"default"'';
    "value.either-object.level" = lib.literalMD "`1` or `2`, depending on the first alternative that parses";
    "value.either-object.mode" = lib.literalMD "`\"a\"` or unspecified, depending on the first alternative that parses";
  };
  wrongDocumentedDefaults = lib.filterAttrs (name: d: d != expectedDocumentedDefaults.${name}) documentedDefaults;
in
if values == expectedValues && unexpectedSuccesses == [ ] && wrongDocumentedDefaults == { }
then runCommand "unset-options-check" { } "touch $out"
else
  throw (builtins.toJSON {
    inherit unexpectedSuccesses wrongDocumentedDefaults;
    # Once these are what they should be, write them to the JSON file.
    wrongValues = lib.filterAttrs (name: v: v != expectedValues.${name} or null) values;
    valuesWithoutDefinition = builtins.attrNames (removeAttrs expectedValues (builtins.attrNames definitions));
  })
