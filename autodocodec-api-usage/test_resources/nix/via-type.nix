{ lib}:
lib.types.submodule {
  options = {
    one = mkOption {
      description = "first field";
      type = lib.types.str;
    };
    two = mkOption {
      description = "second field";
      type = lib.types.str;
    };
  };
}
