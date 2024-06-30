{
  "int" =
    mkOption {
      type = types.s64;
    }
  ;
  "text" =
    mkOption {
      type = types.str;
    }
  ;
  "type" =
    mkOption {
      type = 
        types.oneOf [
          ("that")
          ("both")
          ("this")
        ]
        ;
    }
  ;
}
