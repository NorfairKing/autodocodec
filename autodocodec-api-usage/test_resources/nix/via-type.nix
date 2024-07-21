{ lib }:
lib.types.submodule {
  options = {
    one = lib.mkOption {
      default = null;
      description = "first field";
      type = lib.types.nullOr lib.types.str;
    };
    two = lib.mkOption {
      default = null;
      description = "second field";
      type = lib.types.nullOr lib.types.str;
    };
  };
}
