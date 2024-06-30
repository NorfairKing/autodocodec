mkOption {
  type = 
    attrsOf (types.submodule { options = {
      "bool" =
        mkOption {
          type = types.bool;
        }
      ;
      "fruit" =
        mkOption {
          type = 
            oneOf [
              (types.anything)
              (
                oneOf [
                  (types.anything)
                  (
                    oneOf [
                      (types.anything)
                      (types.anything)
                    ]
                    )
                ]
                )
            ]
            ;
        }
      ;
      "maybe" =
        mkOption {
          type = 
            oneOf [
              (types.anything)
              (types.str)
            ]
            ;
        }
      ;
      "optional" =
        mkOption {
          type = types.str;
        }
      ;
      "optional-or-null" =
        mkOption {
          type = 
            oneOf [
              (types.anything)
              (types.str)
            ]
            ;
        }
      ;
      "optional-with-default" =
        mkOption {
          type = types.str;
        }
      ;
      "optional-with-null-default" =
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
      "single-or-list" =
        mkOption {
          type = 
            oneOf [
              (types.str)
              (
                listOf (
                  mkOption {
                    type = types.str;
                  }
                )
                )
            ]
            ;
        }
      ;
      "text" =
        mkOption {
          type = types.str;
        }
      ;
    }; ))
    ;
  description = "Example";
}
