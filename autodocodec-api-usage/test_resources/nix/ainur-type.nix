{ lib}:
lib.types.oneOf [
  (lib.types.submodule {
    options = {
      domain = mkOption {
        default = null;
        description = "Domain which the Valar rules over";
        type = lib.types.nullOr lib.types.str;
      };
      name = mkOption {
        default = null;
        description = "Name of the Valar";
        type = lib.types.nullOr lib.types.str;
      };
    };
  })
  (lib.types.submodule {
    options = {
      name = mkOption {
        default = null;
        description = "Name of the Maiar";
        type = lib.types.nullOr lib.types.str;
      };
    };
  })
]
