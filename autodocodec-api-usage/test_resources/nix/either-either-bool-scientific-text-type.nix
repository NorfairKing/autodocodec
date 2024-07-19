{ lib}:
lib.types.oneOf  [
  ( lib.types.submodule  {
    options = {
      Left = mkOption  {
        type = lib.types.oneOf  [
          ( lib.types.submodule  {
            options = {
              Left = mkOption  {
                type = lib.types.bool;
              };
            };
          })
          ( lib.types.submodule  {
            options = {
              Right = mkOption  {
                type = lib.types.number;
              };
            };
          })
        ];
      };
    };
  })
  ( lib.types.submodule  {
    options = {
      Right = mkOption  {
        type = lib.types.str;
      };
    };
  })
]
