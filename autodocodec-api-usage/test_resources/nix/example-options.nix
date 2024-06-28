{
  bool = mkOption  {
    description = "a bool";
    type = types.bool;
  };
  fruit = mkOption  {
    description = "fruit!!";
    type = types.anything;
  };
  maybe = mkOption  {
    description = "a maybe text";
    type = types.oneOf  [
      types.anything
      types.str
    ];
  };
  optional = mkOption  {
    description = "an optional text";
    type = types.str;
  };
  optional-or-null = mkOption  {
    description = "an optional-or-null text";
    type = types.oneOf  [
      types.anything
      types.str
    ];
  };
  optional-with-default = mkOption  {
    description = "an optional text with a default";
    type = types.str;
  };
  optional-with-null-default = mkOption  {
    description = "an optional list of texts with a default empty list where the empty list would be omitted";
    type = types.listOf types.str;
  };
  single-or-list = mkOption  {
    description = "an optional list that can also be specified as a single element";
    type = types.oneOf  [
      types.str
      ( types.listOf types.str)
    ];
  };
  text = mkOption  {
    description = "a text";
    type = types.str;
  };
}
