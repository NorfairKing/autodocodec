{ lib }:
{
  optional = lib.mkOption {
    default = null;
    type = lib.types.nullOr lib.types.str;
  };
  optional-or-null = lib.mkOption {
    default = null;
    type = lib.types.nullOr lib.types.str;
  };
  optional-or-null-with = lib.mkOption {
    default = null;
    type = lib.types.nullOr lib.types.str;
  };
  or-null-with-default = lib.mkOption {
    default = "default";
    description = "an or-null field with a default";
    type = lib.types.nullOr lib.types.str;
  };
  or-null-with-default-undocumented = lib.mkOption {
    default = "default";
    type = lib.types.nullOr lib.types.str;
  };
  or-null-with-omitted-default = lib.mkOption {
    default = "default";
    description = "an or-null field with an omitted default";
    type = lib.types.nullOr lib.types.str;
  };
  or-null-with-omitted-default-undocumented = lib.mkOption {
    default = "default";
    type = lib.types.nullOr lib.types.str;
  };
  with-default = lib.mkOption {
    default = "default";
    type = lib.types.str;
  };
  with-omitted-default = lib.mkOption {
    default = "default";
    type = lib.types.str;
  };
}
