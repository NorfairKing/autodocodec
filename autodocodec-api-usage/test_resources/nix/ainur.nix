mkOption {
  type = 
    oneOf [
      (
        attrsOf (types.submodule { options = {
          "domain" =
            mkOption {
              type = types.str;
            }
          ;
          "name" =
            mkOption {
              type = types.str;
            }
          ;
        }; ))
        )
      (
        attrsOf (types.submodule { options = {
          "name" =
            mkOption {
              type = types.str;
            }
          ;
        }; ))
        )
    ]
    ;
}
