{ lib }:
lib.types.oneOf [
  (lib.types.submodule {
    options = {
      Left = lib.mkOption {
        default = null;
        type = lib.types.nullOr (lib.types.oneOf [
          (lib.types.submodule {
            options = {
              Left = lib.mkOption {
                default = null;
                type = lib.types.nullOr lib.types.bool;
              };
            };
          })
          (lib.types.submodule {
            options = {
              Right = lib.mkOption {
                default = null;
                type = lib.types.nullOr lib.types.number;
              };
            };
          })
        ]);
      };
    };
  })
  (lib.types.submodule {
    options = {
      Right = lib.mkOption {
        default = null;
        type = lib.types.nullOr lib.types.str;
      };
    };
  })
]
