{ lib }:
lib.types.submodule ({ config, options, ... }:
{
  options = {
    __defined = lib.mkOption {
      default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
        optional-non-empty = config.optional-non-empty;
        possibly-empty-with-default = config.possibly-empty-with-default;
        possibly-empty-with-omitted-default = config.possibly-empty-with-omitted-default;
        required-non-empty = config.required-non-empty;
      };
      internal = true;
      readOnly = true;
    };
    optional-non-empty = lib.mkOption {
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
      description = "required non-empty list";
      type = lib.types.listOf lib.types.str;
    };
  };
})
