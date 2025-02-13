{ lib }:
lib.types.oneOf [
  (lib.types.submodule {
    options = {
      domain = lib.mkOption {
        description = "Domain which the Valar rules over";
        type = lib.types.str;
      };
      name = lib.mkOption {
        description = "Name of the Valar";
        type = lib.types.str;
      };
    };
  })
  (lib.types.submodule {
    options = {
      name = lib.mkOption {
        description = "Name of the Maiar";
        type = lib.types.str;
      };
    };
  })
]
