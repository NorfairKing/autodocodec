mkOption {
  type = 
    attrsOf (types.submodule { options = {
      "1" =
        mkOption {
          type = types.str;
        }
      ;
      "1old" =
        mkOption {
          type = types.str;
        }
      ;
      "2" =
        mkOption {
          type = types.str;
        }
      ;
      "2old" =
        mkOption {
          type = types.str;
        }
      ;
      "3" =
        mkOption {
          type = types.str;
        }
      ;
      "3old" =
        mkOption {
          type = types.str;
        }
      ;
    }; ))
    ;
  description = "LegacyValue";
}
