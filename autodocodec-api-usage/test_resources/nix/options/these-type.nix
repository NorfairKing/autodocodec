{ lib }:
lib.types.submodule {
  options = {
    int = lib.mkOption {
      type = lib.types.int;
    };
    text = lib.mkOption {
      type = lib.types.str;
    };
    type = lib.mkOption {
      type = lib.types.oneOf [
        "both"
        "this"
        "that"
      ];
    };
  };
}
