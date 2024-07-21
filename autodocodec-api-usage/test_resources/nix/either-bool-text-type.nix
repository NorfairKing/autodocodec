{ lib}:
lib.types.oneOf [
  (lib.types.submodule {
    options = {
      Left = mkOption {
        default = null;
        type = lib.types.nullOr lib.types.bool;
      };
    };
  })
  (lib.types.submodule {
    options = {
      Right = mkOption {
        default = null;
        type = lib.types.nullOr lib.types.str;
      };
    };
  })
]
