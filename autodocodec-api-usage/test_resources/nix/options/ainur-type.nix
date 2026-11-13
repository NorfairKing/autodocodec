{ lib }:
({ definedSubmodule }:
definedSubmodule (lib.types.submodule ({ config, options, ... }:
{
  options = {
    __defined = lib.mkOption {
      default = lib.mergeAttrs {
        name = if options.name.isDefined then config.name else throw "The option `${lib.showOption options.name.loc}' is required but was not defined.";
      } (lib.filterAttrs (name: _: options.${name}.isDefined) {
        domain = config.domain;
      });
      internal = true;
      readOnly = true;
    };
    domain = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      description = "Domain which the Valar rules over";
      type = lib.types.str;
    };
    name = lib.mkOption {
      defaultText = lib.literalMD "None, this option is required.";
      description = "Name of the Maiar";
      type = lib.types.str;
    };
  };
}))) {
  definedSubmodule = let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue = if submodule.emptyValue ? value then { value = submodule.emptyValue.value.__defined; } else submodule.emptyValue; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule;
}
