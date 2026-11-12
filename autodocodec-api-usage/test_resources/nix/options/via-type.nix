{ lib }:
({ definedSubmodule }:
definedSubmodule (lib.types.submodule ({ config, options, ... }:
{
  options = {
    __defined = lib.mkOption {
      default = {
        one = if options.one.isDefined then config.one else throw "The option `${lib.showOption options.one.loc}' is required but was not defined.";
        two = if options.two.isDefined then config.two else throw "The option `${lib.showOption options.two.loc}' is required but was not defined.";
      };
      internal = true;
      readOnly = true;
    };
    one = lib.mkOption {
      defaultText = lib.literalMD "None, this option is required.";
      description = "first field";
      type = lib.types.str;
    };
    two = lib.mkOption {
      defaultText = lib.literalMD "None, this option is required.";
      description = "second field";
      type = lib.types.str;
    };
  };
}))) {
  definedSubmodule = let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue = if submodule.emptyValue ? value then { value = submodule.emptyValue.value.__defined; } else submodule.emptyValue; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule;
}
