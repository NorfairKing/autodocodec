{ lib }:
{
  int = lib.mkOption {
    description = "int for b";
    type = lib.types.int;
  };
  string = lib.mkOption {
    description = "string for a";
    type = lib.types.str;
  };
  type = lib.mkOption {
    type = lib.types.enum [
      "a"
      "b"
    ];
  };
}
