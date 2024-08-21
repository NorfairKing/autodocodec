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
  };
}
