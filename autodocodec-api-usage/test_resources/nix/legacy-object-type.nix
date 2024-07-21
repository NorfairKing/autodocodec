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
    new = mkOption {
      default = null;
      description = "new key";
      type = lib.types.nullOr lib.types.str;
    };
    newer = mkOption {
      default = null;
      description = "newer key";
      type = lib.types.nullOr lib.types.str;
    };
    newest = mkOption {
      default = null;
      description = "newest key";
      type = lib.types.nullOr lib.types.str;
    };
    old = mkOption {
      default = null;
      description = "old key";
      type = lib.types.nullOr lib.types.str;
    };
    older = mkOption {
      default = null;
      description = "older key";
      type = lib.types.nullOr lib.types.str;
    };
    oldest = mkOption {
      default = null;
      description = "oldest key";
      type = lib.types.nullOr lib.types.str;
    };
  };
}
