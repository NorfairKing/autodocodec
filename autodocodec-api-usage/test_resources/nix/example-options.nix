{ lib }:
{
  bool = lib.mkOption {
    default = null;
    description = "a bool";
    type = lib.types.nullOr lib.types.bool;
  };
  fruit = lib.mkOption {
    default = null;
    description = "fruit!!";
    type = lib.types.nullOr (lib.types.enum [
      "Apple"
      "Orange"
      "Banana"
      "Melon"
    ]);
  };
  maybe = lib.mkOption {
    default = null;
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
    default = null;
    description = "a text";
    type = lib.types.nullOr lib.types.str;
  };
}
