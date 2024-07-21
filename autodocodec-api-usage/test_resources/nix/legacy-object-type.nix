{ lib }:
lib.types.submodule {
  options = {
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
    new = lib.mkOption {
      default = null;
      description = "new key";
      type = lib.types.nullOr lib.types.str;
    };
    newer = lib.mkOption {
      default = null;
      description = "newer key";
      type = lib.types.nullOr lib.types.str;
    };
    newest = lib.mkOption {
      default = null;
      description = "newest key";
      type = lib.types.nullOr lib.types.str;
    };
    old = lib.mkOption {
      default = null;
      description = "old key";
      type = lib.types.nullOr lib.types.str;
    };
    older = lib.mkOption {
      default = null;
      description = "older key";
      type = lib.types.nullOr lib.types.str;
    };
    oldest = lib.mkOption {
      default = null;
      description = "oldest key";
      type = lib.types.nullOr lib.types.str;
    };
  };
}
