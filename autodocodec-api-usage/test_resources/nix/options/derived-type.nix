{ lib }:
lib.types.submodule ({ config, options, ... }:
{
  options = {
    __defined = lib.mkOption {
      default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
        bool = config.bool;
        fruit = config.fruit;
        maybe = config.maybe;
        optional = config.optional;
        optional-or-null = config.optional-or-null;
        optional-with-default = config.optional-with-default;
        optional-with-null-default = config.optional-with-null-default;
        shape = config.shape;
        single-or-list = config.single-or-list;
        text = config.text;
      };
      internal = true;
      readOnly = true;
    };
    bool = lib.mkOption {
      description = "a bool";
      type = lib.types.bool;
    };
    fruit = lib.mkOption {
      description = "fruit!!";
      type = lib.types.enum [
        "Apple"
        "Orange"
        "Banana"
        "Melon"
      ];
    };
    maybe = lib.mkOption {
      description = "a maybe text";
      type = lib.types.nullOr lib.types.str;
    };
    optional = lib.mkOption {
      description = "an optional text";
      type = lib.types.str;
    };
    optional-or-null = lib.mkOption {
      description = "an optional-or-null text";
      type = lib.types.nullOr lib.types.str;
    };
    optional-with-default = lib.mkOption {
      default = "foobar";
      description = "an optional text with a default";
      type = lib.types.str;
    };
    optional-with-null-default = lib.mkOption {
      default = [];
      description = "an optional list of texts with a default empty list where the empty list would be omitted";
      type = lib.types.listOf lib.types.str;
    };
    shape = lib.mkOption {
      description = "shape!?";
      type = lib.types.enum [
        "circle"
        "square"
        "rectangle"
      ];
    };
    single-or-list = lib.mkOption {
      default = [];
      description = "an optional list that can also be specified as a single element";
      type = lib.types.oneOf [
        lib.types.str
        (lib.types.listOf lib.types.str)
      ];
    };
    text = lib.mkOption {
      description = "a text";
      type = lib.types.str;
    };
  };
})
