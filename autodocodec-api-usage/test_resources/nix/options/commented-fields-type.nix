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
  "required"
  "undescribed"
] {
  defaulted = lib.mkOption {
    default = "default";
    description = "the defaulted field\nmore about the defaulted field";
    type = lib.types.str;
  };
  optional = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "the optional field\nmore about the optional field\non a second line";
    type = lib.types.str;
  };
  or-null = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "the or-null field\nmore about the or-null field";
    type = lib.types.nullOr lib.types.str;
  };
  required = lib.mkOption {
    defaultText = lib.literalMD "None, required" // {
      required = true;
    };
    description = "the required field\nmore about the required field";
    type = lib.types.str;
  };
  undescribed = lib.mkOption {
    defaultText = lib.literalMD "None, required" // {
      required = true;
    };
    description = "all this field has to say";
    type = lib.types.str;
  };
}
