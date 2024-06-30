types.oneOf [
  (
    types.submodule { options = 
      {
        "Left" =
          mkOption {
            type = 
              types.oneOf [
                (
                  types.submodule { options = 
                    {
                      "Left" =
                        mkOption {
                          type = types.bool;
                        }
                      ;
                    }
                  ;}
                  )
                (
                  types.submodule { options = 
                    {
                      "Right" =
                        mkOption {
                          type = types.number;
                        }
                      ;
                    }
                  ;}
                  )
              ]
              ;
          }
        ;
      }
    ;}
    )
  (
    types.submodule { options = 
      {
        "Right" =
          mkOption {
            type = types.str;
          }
        ;
      }
    ;}
    )
]
