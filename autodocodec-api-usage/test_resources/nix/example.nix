mkOption {
  type = 
    attrsOf (types.submodule { options = {
      bool =
        mkOption {
          type = types.bool;
        }
      ;
      fruit =
        mkOption {
        }
      ;
      maybe =
        mkOption {
        }
      ;
      optional =
        mkOption {
          type = types.str;
        }
      ;
      optional-or-null =
        mkOption {
        }
      ;
      optional-with-default =
        mkOption {
          type = types.str;
        }
      ;
      optional-with-null-default =
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
      single-or-list =
        mkOption {
        }
      ;
      text =
        mkOption {
          type = types.str;
        }
      ;
    }; ))
    ;
  description = "Example";
}
