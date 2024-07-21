{ lib }:
{
  optional-non-empty = lib.mkOption {
    default = null;
    description = "optional non-empty list";
    type = lib.types.nullOr (lib.types.listOf lib.types.str);
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
    default = null;
    description = "required non-empty list";
    type = lib.types.nullOr (lib.types.listOf lib.types.str);
  };
}
