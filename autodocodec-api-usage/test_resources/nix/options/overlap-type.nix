{ lib }:
lib.types.oneOf [
  ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
  {
    options = {
      __defined = lib.mkOption {
        default = {
          text = if options.text.isDefined then config.text else throw "The option `${lib.showOption options.text.loc}' is required but was not defined.";
          type = if options.type.isDefined then config.type else throw "The option `${lib.showOption options.type.loc}' is required but was not defined.";
        };
        internal = true;
        readOnly = true;
      };
      text = lib.mkOption {
        description = "text for a";
        type = lib.types.str;
      };
      type = lib.mkOption {
        type = lib.types.enum ["a"];
      };
    };
  })))
  ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
  {
    options = {
      __defined = lib.mkOption {
        default = {
          int = if options.int.isDefined then config.int else throw "The option `${lib.showOption options.int.loc}' is required but was not defined.";
          type = if options.type.isDefined then config.type else throw "The option `${lib.showOption options.type.loc}' is required but was not defined.";
        };
        internal = true;
        readOnly = true;
      };
      int = lib.mkOption {
        description = "int for b";
        type = lib.types.int;
      };
      type = lib.mkOption {
        type = lib.types.enum ["b"];
      };
    };
  })))
]
