{ lib}:
{
  optional-non-empty = mkOption  {
    description = "optional non-empty list";
    type = lib.types.listOf lib.types.str;
  };
  possibly-empty-with-default = mkOption  {
    description = "possibly empty list with default empty list";
    type = lib.types.listOf lib.types.s64;
  };
  possibly-empty-with-omitted-default = mkOption  {
    description = "possibly empty list with omitted default empty list";
    type = lib.types.listOf lib.types.s64;
  };
  required-non-empty = mkOption  {
    description = "required non-empty list";
    type = lib.types.listOf lib.types.str;
  };
}
