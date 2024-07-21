{ lib}:
lib.types.submodule {
  options = {
    int = mkOption {
      type = lib.types.nullOr lib.types.s64;
    };
    text = mkOption {
      type = lib.types.nullOr lib.types.str;
    };
    type = mkOption {
      type = lib.types.oneOf [
        "that"
        "both"
        "this"
      ];
    };
  };
}
