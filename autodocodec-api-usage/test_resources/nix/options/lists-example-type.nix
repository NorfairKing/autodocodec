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
definedSubmodule ["required-non-empty"] {
  optional-non-empty = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "optional non-empty list";
    type = lib.types.listOf lib.types.str;
  };
  possibly-empty-with-default = lib.mkOption {
    default = [];
    description = "possibly empty list with default empty list";
    type = lib.types.listOf lib.types.int;
  };
  possibly-empty-with-omitted-default = lib.mkOption {
    default = [];
    description = "possibly empty list with omitted default empty list";
    type = lib.types.listOf lib.types.int;
  };
  required-non-empty = lib.mkOption {
    defaultText = lib.literalMD "None, required" // {
      required = true;
    };
    description = "required non-empty list";
    type = lib.types.listOf lib.types.str;
  };
}
