{ lib }:
lib.types.oneOf [
  (lib.types.submodule ({ config, options, ... }:
  {
    options = {
      __defined = lib.mkOption {
        default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
          domain = config.domain;
          name = config.name;
        };
        internal = true;
        readOnly = true;
      };
      domain = lib.mkOption {
        description = "Domain which the Valar rules over";
        type = lib.types.str;
      };
      name = lib.mkOption {
        description = "Name of the Valar";
        type = lib.types.str;
      };
    };
  }))
  (lib.types.submodule ({ config, options, ... }:
  {
    options = {
      __defined = lib.mkOption {
        default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
          name = config.name;
        };
        internal = true;
        readOnly = true;
      };
      name = lib.mkOption {
        description = "Name of the Maiar";
        type = lib.types.str;
      };
    };
  }))
]
