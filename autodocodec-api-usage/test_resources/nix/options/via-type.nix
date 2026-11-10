{ lib }:
lib.types.submodule ({ config, options, ... }:
{
  options = {
    __defined = lib.mkOption {
      default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
        one = config.one;
        two = config.two;
      };
      internal = true;
      readOnly = true;
    };
    one = lib.mkOption {
      description = "first field";
      type = lib.types.str;
    };
    two = lib.mkOption {
      description = "second field";
      type = lib.types.str;
    };
  };
})
