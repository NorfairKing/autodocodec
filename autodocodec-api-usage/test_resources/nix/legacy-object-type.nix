{ lib }:
lib.types.submodule {
  options = {
    1 = lib.mkOption {
      description = "text 1";
      type = lib.types.str;
    };
    1old = lib.mkOption {
      description = "text 1";
      type = lib.types.str;
    };
    2 = lib.mkOption {
      description = "text 2";
      type = lib.types.str;
    };
    2old = lib.mkOption {
      description = "text 2";
      type = lib.types.str;
    };
    3 = lib.mkOption {
      description = "text 3";
      type = lib.types.str;
    };
    3old = lib.mkOption {
      description = "text 3";
      type = lib.types.str;
    };
    new = lib.mkOption {
      description = "new key";
      type = lib.types.str;
    };
    newer = lib.mkOption {
      description = "newer key";
      type = lib.types.str;
    };
    newest = lib.mkOption {
      description = "newest key";
      type = lib.types.str;
    };
    old = lib.mkOption {
      description = "old key";
      type = lib.types.str;
    };
    older = lib.mkOption {
      description = "older key";
      type = lib.types.str;
    };
    oldest = lib.mkOption {
      description = "oldest key";
      type = lib.types.str;
    };
  };
}
