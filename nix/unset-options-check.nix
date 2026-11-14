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
    "keeps a value of the first of two submodules in a oneOf" = {
      definition = required // { either-objects.a = "a"; };
      expected = withDefaults // { either-objects.a = "a"; };
    };
    "keeps a value of the second of two submodules in a oneOf" = {
      definition = required // { either-objects.b = "b"; };
      expected = withDefaults // { either-objects.b = "b"; };
    };
    "keeps a value of a discriminated union" = {
      definition = required // { these = { type = "this"; text = "t"; }; };
      expected = withDefaults // { these = { type = "this"; text = "t"; }; };
    };
    "keeps definitions with a priority" = {
      definition = required // { optional = lib.mkDefault "d"; with-default = lib.mkForce "f"; };
      expected = withDefaults // { optional = "d"; with-default = "f"; };
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
    "fails when the discriminator of a discriminated union is not defined" = required // { these.text = "t"; };
  };
  evaluates = value: (builtins.tryEval (builtins.deepSeq value null)).success;
  failures = lib.filterAttrs (_: c: defined c.definition != c.expected) cases;
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
      "fails when a codec has a key that the generated submodule uses itself" =
        (lib.evalModules {
          modules = [
            { options.value = lib.mkOption { type = import ../autodocodec-api-usage/test_resources/nix/options/reserved-key-type.nix { inherit lib; }; }; }
            { value.__defined = "d"; }
          ];
        }).config.value;
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
    lib.genAttrs [ "value.nullable" "value.optional-or-null" "value.list" "value.with-default" ] defaultOf;
  expectedDocumentedDefaults = {
    "value.nullable" = lib.literalMD "None, this option is required.";
    "value.optional-or-null" = lib.literalMD "None, this option is left out when it is not set.";
    "value.list" = lib.literalMD "None, this option is left out when it is not set.";
    "value.with-default" = lib.literalExpression ''"default"'';
  };
  wrongDocumentedDefaults = lib.filterAttrs (name: d: d != expectedDocumentedDefaults.${name}) documentedDefaults;
in
if failures == { } && unexpectedSuccesses == [ ] && wrongDocumentedDefaults == { }
then runCommand "unset-options-check" { } "touch $out"
else
  throw (builtins.toJSON {
    inherit unexpectedSuccesses wrongDocumentedDefaults;
    failures = lib.mapAttrs
      (_: c: {
        actual = defined c.definition;
        inherit (c) expected;
      })
      failures;
  })
