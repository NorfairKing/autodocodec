{ lib }:
let
  definedSubmodule = required: declarations: definedType (lib.types.submodule ({ config, options, ... }:
  let
    configValue = name: lib.getAttr name config;
    isDefined = name: (lib.getAttr name options).isDefined;
    requiredValue = name: if isDefined name then configValue name else throw (lib.concatStrings [
      "The option `"
      (lib.showOption (lib.getAttr name options).loc)
      "' is required but was not defined."
    ]);
  in
  {
    options = lib.throwIf (lib.hasAttr "__defined" declarations) "autodocodec-nix uses the option name __defined itself." declarations // {
      __defined = lib.mkOption {
        default = lib.genAttrs required requiredValue // lib.genAttrs (lib.filter isDefined (lib.attrNames declarations)) configValue;
        internal = true;
        readOnly = true;
      };
    };
  }));
  definedType = submodule: submodule // {
    emptyValue = if lib.hasAttr "value" submodule.emptyValue then {
      value = submodule.emptyValue.value.__defined;
    } else submodule.emptyValue;
    functor = submodule.functor // {
      name = "definedSubmodule";
    };
    merge = if lib.isAttrs submodule.merge then {
      __functor = self: loc: defs: (self.v2 {
        defs = defs;
        loc = loc;
      }).value;
      v2 = args: let
        merged = submodule.merge.v2 args;
      in
      merged // {
        value = merged.value.__defined;
      };
    } else loc: defs: (submodule.merge loc defs).__defined;
    substSubModules = modules: definedType (submodule.substSubModules modules);
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
    defaultText = lib.literalMD "None, this option is required.";
    description = "a bool";
    type = lib.types.bool;
  };
  fruit = lib.mkOption {
    defaultText = lib.literalMD "None, this option is required.";
    description = "fruit!!";
    type = lib.types.enum [
      "Apple"
      "Orange"
      "Banana"
      "Melon"
    ];
  };
  maybe = lib.mkOption {
    defaultText = lib.literalMD "None, this option is required.";
    description = "a maybe text";
    type = lib.types.nullOr lib.types.str;
  };
  optional = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    description = "an optional text";
    type = lib.types.str;
  };
  optional-or-null = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
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
    defaultText = lib.literalMD "None, this option is required.";
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
    defaultText = lib.literalMD "None, this option is required.";
    description = "a text";
    type = lib.types.str;
  };
}
