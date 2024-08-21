{ lib }:
{
  one = lib.mkOption {
    description = "first field";
    type = lib.types.str;
  };
  two = lib.mkOption {
    description = "second field";
    type = lib.types.str;
  };
}
