{ lib}:
{
  one = mkOption {
    default = null;
    description = "first field";
    type = lib.types.nullOr lib.types.str;
  };
  two = mkOption {
    default = null;
    description = "second field";
    type = lib.types.nullOr lib.types.str;
  };
}
