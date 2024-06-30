{
  "optional-non-empty" =
    mkOption {
      type = types.listOf (types.str);
      description = "optional non-empty list";
    }
  ;
  "possibly-empty-with-default" =
    mkOption {
      type = types.listOf (types.s64);
      description = "possibly empty list with default empty list";
    }
  ;
  "possibly-empty-with-omitted-default" =
    mkOption {
      type = types.listOf (types.s64);
      description = "possibly empty list with omitted default empty list";
    }
  ;
  "required-non-empty" =
    mkOption {
      type = types.listOf (types.str);
      description = "required non-empty list";
    }
  ;
}
