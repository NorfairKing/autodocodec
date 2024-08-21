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
    type = lib.types.int;
  };
}
