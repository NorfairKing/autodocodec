{ lib }:
lib.types.submodule ({ config, options, ... }:
{
  options = {
    __defined = lib.mkOption {
      default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
        int = config.int;
        text = config.text;
        type = config.type;
      };
      internal = true;
      readOnly = true;
    };
    int = lib.mkOption {
      type = lib.types.int;
    };
    text = lib.mkOption {
      type = lib.types.str;
    };
    type = lib.mkOption {
      type = lib.types.oneOf [
        "both"
        "this"
        "that"
      ];
    };
  };
})
