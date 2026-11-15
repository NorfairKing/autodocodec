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
  "1" = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "text 1";
    type = lib.types.str;
  };
  "1old" = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "text 1";
    type = lib.types.str;
  };
  "2" = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "text 2";
    type = lib.types.str;
  };
  "2old" = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "text 2";
    type = lib.types.str;
  };
  "3" = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "text 3";
    type = lib.types.str;
  };
  "3old" = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "text 3";
    type = lib.types.str;
  };
  new = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "new key";
    type = lib.types.str;
  };
  newer = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "newer key";
    type = lib.types.str;
  };
  newest = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "newest key";
    type = lib.types.str;
  };
  old = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "old key";
    type = lib.types.str;
  };
  older = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "older key";
    type = lib.types.str;
  };
  oldest = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    description = "oldest key";
    type = lib.types.str;
  };
}
