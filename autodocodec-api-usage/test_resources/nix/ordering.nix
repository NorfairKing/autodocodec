mkOption {
  type = 
    oneOf [
      (types.anything)
      (
        oneOf [
          (types.anything)
          (types.anything)
        ]
        )
    ]
    ;
}
