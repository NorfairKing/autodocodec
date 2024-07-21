{ lib }:
lib.types.submodule {
  options = {
    optional-non-empty = lib.mkOption {
      default = null;
      description = "optional non-empty list";
      type = lib.types.nullOr (lib.types.listOf lib.types.str);
    };
    possibly-empty-with-default = lib.mkOption {
      default = [];
      description = "possibly empty list with default empty list";
      type = lib.types.listOf lib.types.s64;
    };
    possibly-empty-with-omitted-default = lib.mkOption {
      default = [];
      description = "possibly empty list with omitted default empty list";
      type = lib.types.listOf lib.types.s64;
    };
    required-non-empty = lib.mkOption {
      default = null;
      description = "required non-empty list";
      type = lib.types.nullOr (lib.types.listOf lib.types.str);
    };
  };
}
