{ lib }:
lib.types.nullOr (lib.types.oneOf [
  (lib.types.submodule {
    options = {
      Left = lib.mkOption {
        type = lib.types.bool;
      };
    };
  })
  (lib.types.submodule {
    options = {
      Right = lib.mkOption {
        type = lib.types.str;
      };
    };
  })
])