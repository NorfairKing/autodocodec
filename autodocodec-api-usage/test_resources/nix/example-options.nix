{ lib}:
{
  bool = mkOption {
    default = null;
    description = "a bool";
    type = lib.types.nullOr lib.types.bool;
  };
  fruit = mkOption {
    default = null;
    description = "fruit!!";
    type = lib.types.nullOr lib.types.anything;
  };
  maybe = mkOption {
    default = null;
    description = "a maybe text";
    type = lib.types.nullOr lib.types.str;
  };
  optional = mkOption {
    default = null;
    description = "an optional text";
    type = lib.types.nullOr lib.types.str;
  };
  optional-or-null = mkOption {
    default = null;
    description = "an optional-or-null text";
    type = lib.types.nullOr lib.types.str;
  };
  optional-with-default = mkOption {
    default = "foobar";
    description = "an optional text with a default";
    type = lib.types.str;
  };
  optional-with-null-default = mkOption {
    default = [];
    description = "an optional list of texts with a default empty list where the empty list would be omitted";
    type = lib.types.listOf lib.types.str;
  };
  single-or-list = mkOption {
    default = [];
    description = "an optional list that can also be specified as a single element";
    type = lib.types.oneOf [
      lib.types.str
      (lib.types.listOf lib.types.str)
    ];
  };
  text = mkOption {
    default = null;
    description = "a text";
    type = lib.types.nullOr lib.types.str;
  };
}
