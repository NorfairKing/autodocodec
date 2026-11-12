{ lib }:
lib.types.oneOf [
  ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
  {
    options = {
      __defined = lib.mkOption {
        default = {
          domain = if options.domain.isDefined then config.domain else throw "The option `${lib.showOption options.domain.loc}' is required but was not defined.";
          name = if options.name.isDefined then config.name else throw "The option `${lib.showOption options.name.loc}' is required but was not defined.";
        };
        internal = true;
        readOnly = true;
      };
      domain = lib.mkOption {
        description = "Domain which the Valar rules over";
        type = lib.types.str;
      };
      name = lib.mkOption {
        description = "Name of the Valar";
        type = lib.types.str;
      };
    };
  })))
  ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
  {
    options = {
      __defined = lib.mkOption {
        default = {
          name = if options.name.isDefined then config.name else throw "The option `${lib.showOption options.name.loc}' is required but was not defined.";
        };
        internal = true;
        readOnly = true;
      };
      name = lib.mkOption {
        description = "Name of the Maiar";
        type = lib.types.str;
      };
    };
  })))
]
