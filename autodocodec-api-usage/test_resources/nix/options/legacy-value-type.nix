{ lib }:
lib.types.submodule ({ config, options, ... }:
{
  options = {
    "1" = lib.mkOption {
      description = "text 1";
      type = lib.types.str;
    };
    "1old" = lib.mkOption {
      description = "text 1";
      type = lib.types.str;
    };
    "2" = lib.mkOption {
      description = "text 2";
      type = lib.types.str;
    };
    "2old" = lib.mkOption {
      description = "text 2";
      type = lib.types.str;
    };
    "3" = lib.mkOption {
      description = "text 3";
      type = lib.types.str;
    };
    "3old" = lib.mkOption {
      description = "text 3";
      type = lib.types.str;
    };
    __defined = lib.mkOption {
      default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
        "1" = config."1";
        "1old" = config."1old";
        "2" = config."2";
        "2old" = config."2old";
        "3" = config."3";
        "3old" = config."3old";
      };
      internal = true;
      readOnly = true;
    };
  };
})
