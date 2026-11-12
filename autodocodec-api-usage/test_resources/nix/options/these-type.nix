{ lib }:
(let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
{
  options = {
    __defined = lib.mkOption {
      default = (lib.mergeAttrs {
        type = if options.type.isDefined then config.type else throw "The option `${lib.showOption options.type.loc}' is required but was not defined.";
      }) ((lib.filterAttrs (name: _: options.${name}.isDefined)) {
        int = config.int;
        text = config.text;
      });
      internal = true;
      readOnly = true;
    };
    int = lib.mkOption {
      type = lib.types.int;
    };
    text = lib.mkOption {
      type = lib.types.str;
    };
    type = lib.mkOption {
      type = lib.types.oneOf [
        "both"
        "this"
        "that"
      ];
    };
  };
}))
