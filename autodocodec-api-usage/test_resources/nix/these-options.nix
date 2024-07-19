{ lib}:
{
  int = mkOption  {
    type = lib.types.s64;
  };
  text = mkOption  {
    type = lib.types.str;
  };
  type = mkOption  {
    type = lib.types.oneOf  [
      "that"
      "both"
      "this"
    ];
  };
}
