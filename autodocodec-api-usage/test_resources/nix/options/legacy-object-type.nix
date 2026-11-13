{ lib }:
({ definedSubmodule }:
definedSubmodule (lib.types.submodule ({ config, options, ... }:
{
  options = {
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
    __defined = lib.mkOption {
      default = lib.filterAttrs (name: _: options.${name}.isDefined) {
        "1" = config."1";
        "1old" = config."1old";
        "2" = config."2";
        "2old" = config."2old";
        "3" = config."3";
        "3old" = config."3old";
        new = config.new;
        newer = config.newer;
        newest = config.newest;
        old = config.old;
        older = config.older;
        oldest = config.oldest;
      };
      internal = true;
      readOnly = true;
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
  };
}))) {
  definedSubmodule = let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue = if submodule.emptyValue ? value then { value = submodule.emptyValue.value.__defined; } else submodule.emptyValue; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule;
}
