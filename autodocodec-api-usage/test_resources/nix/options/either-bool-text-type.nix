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
definedSubmodule [] {
  Left = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = lib.types.bool;
  };
  Right = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = lib.types.str;
  };
}
