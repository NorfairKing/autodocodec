types.oneOf [
  (
    types.submodule {
      options =
        {
          "domain" =
            mkOption {
              type = types.str;
              description = "Domain which the Valar rules over";
            }
          ;
          "name" =
            mkOption {
              type = types.str;
              description = "Name of the Valar";
            }
          ;
        };
    }
  )
  (
    types.submodule {
      options =
        {
          "name" =
            mkOption {
              type = types.str;
              description = "Name of the Maiar";
            }
          ;
        };
    }
  )
]
