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
definedSubmodule [] {
  optional = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    type = lib.types.str;
  };
  optional-or-null = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    type = lib.types.nullOr lib.types.str;
  };
  optional-or-null-with = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    type = lib.types.nullOr lib.types.str;
  };
  or-null-with-default = lib.mkOption {
    default = "default";
    description = "an or-null field with a default";
    type = lib.types.nullOr lib.types.str;
  };
  or-null-with-default-undocumented = lib.mkOption {
    default = "default";
    type = lib.types.nullOr lib.types.str;
  };
  or-null-with-omitted-default = lib.mkOption {
    default = "default";
    description = "an or-null field with an omitted default";
    type = lib.types.nullOr lib.types.str;
  };
  or-null-with-omitted-default-undocumented = lib.mkOption {
    default = "default";
    type = lib.types.nullOr lib.types.str;
  };
  with-default = lib.mkOption {
    default = "default";
    type = lib.types.str;
  };
  with-omitted-default = lib.mkOption {
    default = "default";
    type = lib.types.str;
  };
}
