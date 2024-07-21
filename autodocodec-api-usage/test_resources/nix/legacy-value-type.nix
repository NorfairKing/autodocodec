{ lib}:
lib.types.submodule {
  options = {
    1 = mkOption {
      default = null;
      description = "text 1";
      type = lib.types.nullOr lib.types.str;
    };
    1old = mkOption {
      default = null;
      description = "text 1";
      type = lib.types.nullOr lib.types.str;
    };
    2 = mkOption {
      default = null;
      description = "text 2";
      type = lib.types.nullOr lib.types.str;
    };
    2old = mkOption {
      default = null;
      description = "text 2";
      type = lib.types.nullOr lib.types.str;
    };
    3 = mkOption {
      default = null;
      description = "text 3";
      type = lib.types.nullOr lib.types.str;
    };
    3old = mkOption {
      default = null;
      description = "text 3";
      type = lib.types.nullOr lib.types.str;
    };
  };
}
