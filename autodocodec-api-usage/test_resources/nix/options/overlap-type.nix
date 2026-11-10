{ lib }:
lib.types.oneOf [
  (lib.types.submodule ({ config, options, ... }:
  {
    options = {
      __defined = lib.mkOption {
        default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
          text = config.text;
          type = config.type;
        };
        internal = true;
        readOnly = true;
      };
      text = lib.mkOption {
        description = "text for a";
        type = lib.types.str;
      };
      type = lib.mkOption {
        type = lib.types.enum ["a"];
      };
    };
  }))
  (lib.types.submodule ({ config, options, ... }:
  {
    options = {
      __defined = lib.mkOption {
        default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
          int = config.int;
          type = config.type;
        };
        internal = true;
        readOnly = true;
      };
      int = lib.mkOption {
        description = "int for b";
        type = lib.types.int;
      };
      type = lib.mkOption {
        type = lib.types.enum ["b"];
      };
    };
  }))
]
