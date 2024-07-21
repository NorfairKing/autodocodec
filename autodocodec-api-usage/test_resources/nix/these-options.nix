{ lib }:
{
  int = lib.mkOption {
    type = lib.types.nullOr lib.types.int;
  };
  text = lib.mkOption {
    type = lib.types.nullOr lib.types.str;
  };
  type = lib.mkOption {
    type = lib.types.oneOf [
      "that"
      "both"
      "this"
    ];
  };
}
