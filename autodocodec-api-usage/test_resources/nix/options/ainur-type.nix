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
definedSubmodule ["name"] {
  domain = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "Domain which the Valar rules over";
    type = lib.types.str;
  };
  name = lib.mkOption {
    defaultText = lib.literalMD "None, required" // {
      required = true;
    };
    description = "Name of the Maiar";
    type = lib.types.str;
  };
}
