{ lib}:
{
  left = mkOption {
    type = lib.types.anything;
  };
  right = mkOption {
    type = lib.types.anything;
  };
  type = mkOption {
    type = lib.types.oneOf [
      "sum"
      "product"
      "literal"
    ];
  };
  value = mkOption {
    default = null;
    type = lib.types.nullOr lib.types.s64;
  };
}
