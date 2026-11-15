{ lib }:
let
  definedSubmodule = required: declarations: definedType required (lib.types.submodule {
    options = declarations;
  });
  definedType = required: submodule: submodule // {
    emptyValue = { };
    functor = submodule.functor // {
      name = "definedSubmodule";
    };
    merge = submodule.merge // {
      v2 = args: let
        configValue = name: lib.getAttr name merged.value;
        isDefined = name: (lib.getAttr name options).isDefined;
        merged = submodule.merge.v2 args;
        options = merged.valueMeta.configuration.options;
        requiredValue = name: if isDefined name then configValue name else throw (lib.concatStrings [
          "The option `"
          (lib.showOption (lib.getAttr name options).loc)
          "' is required but was not defined."
        ]);
      in
      merged // {
        value = lib.genAttrs required requiredValue // lib.genAttrs (lib.filter isDefined (lib.attrNames (lib.removeAttrs options ["_module"]))) configValue;
      };
    };
    substSubModules = modules: definedType required (submodule.substSubModules modules);
    typeMerge = _: null;
  };
in
definedSubmodule [
  "bool"
  "fruit"
  "maybe"
  "shape"
  "text"
] {
  bool = lib.mkOption {
    defaultText = lib.literalMD "None, required" // {
      required = true;
    };
    description = "a bool";
    type = lib.types.bool;
  };
  fruit = lib.mkOption {
    defaultText = lib.literalMD "None, required" // {
      required = true;
    };
    description = "fruit!!";
    type = lib.types.enum [
      "Apple"
      "Orange"
      "Banana"
      "Melon"
    ];
  };
  maybe = lib.mkOption {
    defaultText = lib.literalMD "None, required" // {
      required = true;
    };
    description = "a maybe text";
    type = lib.types.nullOr lib.types.str;
  };
  optional = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "an optional text";
    type = lib.types.str;
  };
  optional-or-null = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "an optional-or-null text";
    type = lib.types.nullOr lib.types.str;
  };
  optional-with-default = lib.mkOption {
    default = "foobar";
    description = "an optional text with a default";
    type = lib.types.str;
  };
  optional-with-null-default = lib.mkOption {
    default = [];
    description = "an optional list of texts with a default empty list where the empty list would be omitted";
    type = lib.types.listOf lib.types.str;
  };
  shape = lib.mkOption {
    defaultText = lib.literalMD "None, required" // {
      required = true;
    };
    description = "shape!?";
    type = lib.types.enum [
      "circle"
      "square"
      "rectangle"
    ];
  };
  single-or-list = lib.mkOption {
    default = [];
    description = "an optional list that can also be specified as a single element";
    type = lib.types.oneOf [
      lib.types.str
      (lib.types.listOf lib.types.str)
    ];
  };
  text = lib.mkOption {
    defaultText = lib.literalMD "None, required" // {
      required = true;
    };
    description = "a text";
    type = lib.types.str;
  };
}
