{ lib}:
lib.types.oneOf  [
  ( lib.types.submodule  {
    options = {
      domain = mkOption  {
        description = "Domain which the Valar rules over";
        type = lib.types.str;
      };
      name = mkOption  {
        description = "Name of the Valar";
        type = lib.types.str;
      };
    };
  })
  ( lib.types.submodule  {
    options = {
      name = mkOption  {
        description = "Name of the Maiar";
        type = lib.types.str;
      };
    };
  })
]
