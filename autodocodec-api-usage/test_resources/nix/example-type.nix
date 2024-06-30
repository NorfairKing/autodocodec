types.submodule { options = 
  {
    "bool" =
      mkOption {
        type = types.bool;
        description = "a bool";
      }
    ;
    "fruit" =
      mkOption {
        type = 
          oneOf [
            (types.anything)
            (types.anything)
            (types.anything)
            (types.anything)
          ]
          ;
        description = "fruit!!";
      }
    ;
    "maybe" =
      mkOption {
        type = 
          oneOf [
            (types.anything)
            (types.str)
          ]
          ;
        description = "a maybe text";
      }
    ;
    "optional" =
      mkOption {
        type = types.str;
        description = "an optional text";
      }
    ;
    "optional-or-null" =
      mkOption {
        type = 
          oneOf [
            (types.anything)
            (types.str)
          ]
          ;
        description = "an optional-or-null text";
      }
    ;
    "optional-with-default" =
      mkOption {
        type = types.str;
        description = "an optional text with a default";
      }
    ;
    "optional-with-null-default" =
      mkOption {
        type = listOf (types.str);
        description = "an optional list of texts with a default empty list where the empty list would be omitted";
      }
    ;
    "single-or-list" =
      mkOption {
        type = 
          oneOf [
            (types.str)
            (listOf (types.str))
          ]
          ;
        description = "an optional list that can also be specified as a single element";
      }
    ;
    "text" =
      mkOption {
        type = types.str;
        description = "a text";
      }
    ;
  }
;}
