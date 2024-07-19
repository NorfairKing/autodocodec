{ lib }:
{
  left = lib.mkOption {
    type = lib.types.anything;
  };
  right = lib.mkOption {
    type = lib.types.anything;
  };
  type = lib.mkOption {
    type = lib.types.oneOf [
      "sum"
      "product"
      "literal"
    ];
  };
  value = lib.mkOption {
    default = null;
    type = lib.types.nullOr lib.types.s64;
  };
}
