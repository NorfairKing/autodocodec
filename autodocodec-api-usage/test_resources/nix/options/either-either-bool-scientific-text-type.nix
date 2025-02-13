{ lib }:
lib.types.oneOf [
  (lib.types.submodule {
    options = {
      Left = lib.mkOption {
        type = lib.types.oneOf [
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
                type = lib.types.number;
              };
            };
          })
        ];
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
]
