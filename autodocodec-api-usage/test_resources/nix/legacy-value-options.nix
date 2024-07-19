{ lib }:
{
  1 = lib.mkOption {
    default = null;
    description = "text 1";
    type = lib.types.nullOr lib.types.str;
  };
  1old = lib.mkOption {
    default = null;
    description = "text 1";
    type = lib.types.nullOr lib.types.str;
  };
  2 = lib.mkOption {
    default = null;
    description = "text 2";
    type = lib.types.nullOr lib.types.str;
  };
  2old = lib.mkOption {
    default = null;
    description = "text 2";
    type = lib.types.nullOr lib.types.str;
  };
  3 = lib.mkOption {
    default = null;
    description = "text 3";
    type = lib.types.nullOr lib.types.str;
  };
  3old = lib.mkOption {
    default = null;
    description = "text 3";
    type = lib.types.nullOr lib.types.str;
  };
}
