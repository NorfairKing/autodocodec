oneOf [
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
            type = types.str;
          }
        ;
      }
    ;}
    )
]
