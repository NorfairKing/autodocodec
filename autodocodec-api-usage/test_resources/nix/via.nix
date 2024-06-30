mkOption {
  type = 
    attrsOf (types.submodule { options = {
      "one" =
        mkOption {
          type = types.str;
        }
      ;
      "two" =
        mkOption {
          type = types.str;
        }
      ;
    }; ))
    ;
  description = "Via";
}
