{ lib }:
lib.types.submodule {
  options = {
    defaulted = lib.mkOption {
      default = "default";
      description = "the defaulted field\nmore about the defaulted field";
      type = lib.types.str;
    };
    optional = lib.mkOption {
      default = null;
      description = "the optional field\nmore about the optional field\non a second line";
      type = lib.types.nullOr lib.types.str;
    };
    or-null = lib.mkOption {
      default = null;
      description = "the or-null field\nmore about the or-null field";
      type = lib.types.nullOr lib.types.str;
    };
    required = lib.mkOption {
      description = "the required field\nmore about the required field";
      type = lib.types.str;
    };
    undescribed = lib.mkOption {
      description = "all this field has to say";
      type = lib.types.str;
    };
  };
}
