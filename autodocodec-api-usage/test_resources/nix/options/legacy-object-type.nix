{ lib }:
let
  definedSubmodule = required: declarations: lib.throwIf (lib.hasAttr "__defined" declarations) "autodocodec-nix uses the option name __defined itself." (definedType (lib.types.submodule ({ config, options, ... }:
  {
    options = declarations // {
      __defined = lib.mkOption {
        default = lib.genAttrs required (name: if (lib.getAttr name options).isDefined then lib.getAttr name config else throw (lib.concatStrings [
          "The option `"
          (lib.showOption (lib.getAttr name options).loc)
          "' is required but was not defined."
        ])) // lib.mapAttrs (name: _: lib.getAttr name config) (lib.filterAttrs (name: _: (lib.getAttr name options).isDefined) declarations);
        internal = true;
        readOnly = true;
      };
    };
  })));
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
  "1" = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    description = "text 1";
    type = lib.types.str;
  };
  "1old" = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    description = "text 1";
    type = lib.types.str;
  };
  "2" = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    description = "text 2";
    type = lib.types.str;
  };
  "2old" = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    description = "text 2";
    type = lib.types.str;
  };
  "3" = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    description = "text 3";
    type = lib.types.str;
  };
  "3old" = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    description = "text 3";
    type = lib.types.str;
  };
  new = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    description = "new key";
    type = lib.types.str;
  };
  newer = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    description = "newer key";
    type = lib.types.str;
  };
  newest = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    description = "newest key";
    type = lib.types.str;
  };
  old = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    description = "old key";
    type = lib.types.str;
  };
  older = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    description = "older key";
    type = lib.types.str;
  };
  oldest = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    description = "oldest key";
    type = lib.types.str;
  };
}
