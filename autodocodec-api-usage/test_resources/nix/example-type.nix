{ lib}:
lib.types.submodule {
  options = {
    bool = mkOption {
      description = "a bool";
      type = lib.types.bool;
    };
    fruit = mkOption {
      description = "fruit!!";
      type = lib.types.anything;
    };
    maybe = mkOption {
      description = "a maybe text";
      type = lib.types.oneOf [
        lib.types.anything
        lib.types.str
      ];
    };
    optional = mkOption {
      description = "an optional text";
      type = lib.types.str;
    };
    optional-or-null = mkOption {
      description = "an optional-or-null text";
      type = lib.types.oneOf [
        lib.types.anything
        lib.types.str
      ];
    };
    optional-with-default = mkOption {
      description = "an optional text with a default";
      type = lib.types.str;
    };
    optional-with-null-default = mkOption {
      description = "an optional list of texts with a default empty list where the empty list would be omitted";
      type = lib.types.listOf lib.types.str;
    };
    single-or-list = mkOption {
      description = "an optional list that can also be specified as a single element";
      type = lib.types.oneOf [
        lib.types.str
        (lib.types.listOf lib.types.str)
      ];
    };
    text = mkOption {
      description = "a text";
      type = lib.types.str;
    };
  };
}
