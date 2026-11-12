{ lib }:
({ definedSubmodule }:
lib.types.nullOr (definedSubmodule (lib.types.submodule ({ config, options, ... }:
{
  options = {
    Left = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.bool;
    };
    Right = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.str;
    };
    __defined = lib.mkOption {
      default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
        Left = config.Left;
        Right = config.Right;
      };
      internal = true;
      readOnly = true;
    };
  };
})))) {
  definedSubmodule = let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue = if submodule.emptyValue ? value then { value = submodule.emptyValue.value.__defined; } else submodule.emptyValue; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule;
}
