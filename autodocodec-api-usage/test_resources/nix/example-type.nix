{ lib }:
lib.types.submodule {
  options = {
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
      default = null;
      description = "an optional text";
      type = lib.types.nullOr lib.types.str;
    };
    optional-or-null = lib.mkOption {
      default = null;
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
}
