mkOption {
  type = 
    attrsOf (types.submodule { options = {
      optional-non-empty =
        mkOption {
          type = 
            listOf (
              mkOption {
                type = types.str;
              }
            )
            ;
        }
      ;
      possibly-empty-with-default =
        mkOption {
          type = 
            listOf (
              mkOption {
                type = types.s64;
              }
            )
            ;
        }
      ;
      possibly-empty-with-omitted-default =
        mkOption {
          type = 
            listOf (
              mkOption {
                type = types.s64;
              }
            )
            ;
        }
      ;
      required-non-empty =
        mkOption {
          type = 
            listOf (
              mkOption {
                type = types.str;
              }
            )
            ;
        }
      ;
    }; ))
    ;
  description = "ListsExample";
}
