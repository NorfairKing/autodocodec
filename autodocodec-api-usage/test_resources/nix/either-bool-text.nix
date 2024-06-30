mkOption {
  type = 
    oneOf [
      (
        attrsOf (types.submodule { options = {
          "Left" =
            mkOption {
              type = types.bool;
            }
          ;
        }; ))
        )
      (
        attrsOf (types.submodule { options = {
          "Right" =
            mkOption {
              type = types.str;
            }
          ;
        }; ))
        )
    ]
    ;
}
