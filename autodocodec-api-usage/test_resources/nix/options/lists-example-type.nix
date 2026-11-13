{ lib }:
({ definedSubmodule }:
definedSubmodule (lib.types.submodule ({ config, options, ... }:
{
  options = {
    __defined = lib.mkOption {
      default = lib.mergeAttrs {
        required-non-empty = if options.required-non-empty.isDefined then config.required-non-empty else throw "The option `${lib.showOption options.required-non-empty.loc}' is required but was not defined.";
      } (lib.filterAttrs (name: _: options.${name}.isDefined) {
        optional-non-empty = config.optional-non-empty;
        possibly-empty-with-default = config.possibly-empty-with-default;
        possibly-empty-with-omitted-default = config.possibly-empty-with-omitted-default;
      });
      internal = true;
      readOnly = true;
    };
    optional-non-empty = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      description = "optional non-empty list";
      type = lib.types.listOf lib.types.str;
    };
    possibly-empty-with-default = lib.mkOption {
      default = [];
      description = "possibly empty list with default empty list";
      type = lib.types.listOf lib.types.int;
    };
    possibly-empty-with-omitted-default = lib.mkOption {
      default = [];
      description = "possibly empty list with omitted default empty list";
      type = lib.types.listOf lib.types.int;
    };
    required-non-empty = lib.mkOption {
      defaultText = lib.literalMD "None, this option is required.";
      description = "required non-empty list";
      type = lib.types.listOf lib.types.str;
    };
  };
}))) {
  definedSubmodule = let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue = if submodule.emptyValue ? value then { value = submodule.emptyValue.value.__defined; } else submodule.emptyValue; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule;
}
