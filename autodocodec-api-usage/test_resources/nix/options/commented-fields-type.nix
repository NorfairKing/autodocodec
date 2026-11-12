{ lib }:
(let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
{
  options = {
    __defined = lib.mkOption {
      default = (lib.mergeAttrs {
        required = if options.required.isDefined then config.required else throw "The option `${lib.showOption options.required.loc}' is required but was not defined.";
        undescribed = if options.undescribed.isDefined then config.undescribed else throw "The option `${lib.showOption options.undescribed.loc}' is required but was not defined.";
      }) ((lib.filterAttrs (name: _: options.${name}.isDefined)) {
        defaulted = config.defaulted;
        optional = config.optional;
        or-null = config.or-null;
      });
      internal = true;
      readOnly = true;
    };
    defaulted = lib.mkOption {
      default = "default";
      description = "the defaulted field\nmore about the defaulted field";
      type = lib.types.str;
    };
    optional = lib.mkOption {
      description = "the optional field\nmore about the optional field\non a second line";
      type = lib.types.str;
    };
    or-null = lib.mkOption {
      description = "the or-null field\nmore about the or-null field";
      type = lib.types.nullOr lib.types.str;
    };
    required = lib.mkOption {
      description = "the required field\nmore about the required field";
      type = lib.types.str;
    };
    undescribed = lib.mkOption {
      description = "all this field has to say";
      type = lib.types.str;
    };
  };
}))
