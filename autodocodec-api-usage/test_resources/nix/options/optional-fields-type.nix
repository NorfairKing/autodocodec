{ lib }:
({ definedSubmodule }:
definedSubmodule (lib.types.submodule ({ config, options, ... }:
{
  options = {
    __defined = lib.mkOption {
      default = lib.filterAttrs (name: _: options.${name}.isDefined) {
        optional = config.optional;
        optional-or-null = config.optional-or-null;
        optional-or-null-with = config.optional-or-null-with;
        or-null-with-default = config.or-null-with-default;
        or-null-with-default-undocumented = config.or-null-with-default-undocumented;
        or-null-with-omitted-default = config.or-null-with-omitted-default;
        or-null-with-omitted-default-undocumented = config.or-null-with-omitted-default-undocumented;
        with-default = config.with-default;
        with-omitted-default = config.with-omitted-default;
      };
      internal = true;
      readOnly = true;
    };
    optional = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.str;
    };
    optional-or-null = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.nullOr lib.types.str;
    };
    optional-or-null-with = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.nullOr lib.types.str;
    };
    or-null-with-default = lib.mkOption {
      default = "default";
      description = "an or-null field with a default";
      type = lib.types.nullOr lib.types.str;
    };
    or-null-with-default-undocumented = lib.mkOption {
      default = "default";
      type = lib.types.nullOr lib.types.str;
    };
    or-null-with-omitted-default = lib.mkOption {
      default = "default";
      description = "an or-null field with an omitted default";
      type = lib.types.nullOr lib.types.str;
    };
    or-null-with-omitted-default-undocumented = lib.mkOption {
      default = "default";
      type = lib.types.nullOr lib.types.str;
    };
    with-default = lib.mkOption {
      default = "default";
      type = lib.types.str;
    };
    with-omitted-default = lib.mkOption {
      default = "default";
      type = lib.types.str;
    };
  };
}))) {
  definedSubmodule = let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue = if submodule.emptyValue ? value then { value = submodule.emptyValue.value.__defined; } else submodule.emptyValue; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule;
}
