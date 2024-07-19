(types.oneOf)
(
  [
    (
      (types.submodule)
      (
        {
          options = 
            {
              domain = 
                (mkOption)
                (
                  {
                    description = "Domain which the Valar rules over";
                    type = types.str;
                  }
                  )
              ;
              name = 
                (mkOption)
                (
                  {
                    description = "Name of the Valar";
                    type = types.str;
                  }
                  )
              ;
            }
          ;
        }
        )
      )
    (
      (types.submodule)
      (
        {
          options = 
            {
              name = 
                (mkOption)
                (
                  {
                    description = "Name of the Maiar";
                    type = types.str;
                  }
                  )
              ;
            }
          ;
        }
        )
      )
  ]
  )
