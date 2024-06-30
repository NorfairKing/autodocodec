mkOption {
  type = 
    attrsOf (types.submodule { options = {
      1 =
        mkOption {
          type = types.str;
        }
      ;
      1old =
        mkOption {
          type = types.str;
        }
      ;
      2 =
        mkOption {
          type = types.str;
        }
      ;
      2old =
        mkOption {
          type = types.str;
        }
      ;
      3 =
        mkOption {
          type = types.str;
        }
      ;
      3old =
        mkOption {
          type = types.str;
        }
      ;
      new =
        mkOption {
          type = types.str;
        }
      ;
      newer =
        mkOption {
          type = types.str;
        }
      ;
      newest =
        mkOption {
          type = types.str;
        }
      ;
      old =
        mkOption {
          type = types.str;
        }
      ;
      older =
        mkOption {
          type = types.str;
        }
      ;
      oldest =
        mkOption {
          type = types.str;
        }
      ;
    }; ))
    ;
  description = "LegacyObject";
}
