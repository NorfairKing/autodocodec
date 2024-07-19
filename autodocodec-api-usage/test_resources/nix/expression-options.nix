{
  left = 
    (mkOption)
    ({type = types.anything;})
  ;
  right = 
    (mkOption)
    ({type = types.anything;})
  ;
  type = 
    (mkOption)
    (
      {
        type = 
          (types.oneOf)
          (
            [
              ("sum")
              ("product")
              ("literal")
            ]
            )
        ;
      }
      )
  ;
  value = 
    (mkOption)
    ({type = types.s64;})
  ;
}
