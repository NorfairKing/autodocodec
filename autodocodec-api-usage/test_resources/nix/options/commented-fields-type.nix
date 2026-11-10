{ lib }:
lib.types.submodule ({ config, options, ... }:
{
  options = {
    __defined = lib.mkOption {
      default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
        defaulted = config.defaulted;
        optional = config.optional;
        or-null = config.or-null;
        required = config.required;
        undescribed = config.undescribed;
      };
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
})
