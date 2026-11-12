{ lib }:
lib.types.nullOr (lib.types.oneOf [
  ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
  {
    options = {
      Left = lib.mkOption {
        type = lib.types.bool;
      };
      __defined = lib.mkOption {
        default = {
          Left = if options.Left.isDefined then config.Left else throw "The option `${lib.showOption options.Left.loc}' is required but was not defined.";
        };
        internal = true;
        readOnly = true;
      };
    };
  })))
  ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
  {
    options = {
      Right = lib.mkOption {
        type = lib.types.str;
      };
      __defined = lib.mkOption {
        default = {
          Right = if options.Right.isDefined then config.Right else throw "The option `${lib.showOption options.Right.loc}' is required but was not defined.";
        };
        internal = true;
        readOnly = true;
      };
    };
  })))
])
