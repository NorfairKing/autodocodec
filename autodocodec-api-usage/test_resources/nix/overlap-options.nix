{ lib }:
{
  int = lib.mkOption {
    default = null;
    description = "int for b";
    type = lib.types.nullOr lib.types.int;
  };
  string = lib.mkOption {
    default = null;
    description = "string for a";
    type = lib.types.nullOr lib.types.str;
  };
  type = lib.mkOption {
    default = null;
    type = lib.types.nullOr (lib.types.enum [
      "a"
      "b"
    ]);
  };
}
