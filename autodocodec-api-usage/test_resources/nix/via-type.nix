(types.submodule)
(
  {
    options = 
      {
        one = 
          (mkOption)
          (
            {
              description = "first field";
              type = types.str;
            }
            )
        ;
        two = 
          (mkOption)
          (
            {
              description = "second field";
              type = types.str;
            }
            )
        ;
      }
    ;
  }
  )
