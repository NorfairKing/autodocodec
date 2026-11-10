{ lib }:
lib.types.nullOr (lib.types.oneOf [
  (lib.types.submodule ({ config, options, ... }:
  {
    options = {
      Left = lib.mkOption {
        type = lib.types.bool;
      };
      __defined = lib.mkOption {
        default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
          Left = config.Left;
        };
        internal = true;
        readOnly = true;
      };
    };
  }))
  (lib.types.submodule ({ config, options, ... }:
  {
    options = {
      Right = lib.mkOption {
        type = lib.types.str;
      };
      __defined = lib.mkOption {
        default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
          Right = config.Right;
        };
        internal = true;
        readOnly = true;
      };
    };
  }))
])
