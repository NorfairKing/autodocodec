{ lib}:
{
  1 = mkOption  {
    description = "text 1";
    type = lib.types.str;
  };
  1old = mkOption  {
    description = "text 1";
    type = lib.types.str;
  };
  2 = mkOption  {
    description = "text 2";
    type = lib.types.str;
  };
  2old = mkOption  {
    description = "text 2";
    type = lib.types.str;
  };
  3 = mkOption  {
    description = "text 3";
    type = lib.types.str;
  };
  3old = mkOption  {
    description = "text 3";
    type = lib.types.str;
  };
  new = mkOption  {
    description = "new key";
    type = lib.types.str;
  };
  newer = mkOption  {
    description = "newer key";
    type = lib.types.str;
  };
  newest = mkOption  {
    description = "newest key";
    type = lib.types.str;
  };
  old = mkOption  {
    description = "old key";
    type = lib.types.str;
  };
  older = mkOption  {
    description = "older key";
    type = lib.types.str;
  };
  oldest = mkOption  {
    description = "oldest key";
    type = lib.types.str;
  };
}
