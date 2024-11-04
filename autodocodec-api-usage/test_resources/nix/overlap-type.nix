{ lib }:
lib.types.oneOf [
  (lib.types.submodule {
    options = {
      text = lib.mkOption {
        description = "text for a";
        type = lib.types.str;
      };
      type = lib.mkOption {
        type = lib.types.enum ["a"];
      };
    };
  })
  (lib.types.submodule {
    options = {
      int = lib.mkOption {
        description = "int for b";
        type = lib.types.int;
      };
      type = lib.mkOption {
        type = lib.types.enum ["b"];
      };
    };
  })
]
