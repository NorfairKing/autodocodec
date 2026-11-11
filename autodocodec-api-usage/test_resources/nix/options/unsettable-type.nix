{ lib }:
lib.types.submodule ({ config, options, ... }:
{
  options = {
    "1st" = lib.mkOption {
      type = lib.types.str;
    };
    __defined = lib.mkOption {
      default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
        "1st" = config."1st";
        either = if config.either ? __defined then config.either.__defined else config.either;
        either-list = if config.either-list ? __defined then config.either-list.__defined else if builtins.isList config.either-list then map (v: v.__defined) config.either-list else config.either-list;
        either-object = config.either-object.__defined;
        list = map (v: v.__defined) config.list;
        list-of-lists = map (v: map (v: v.__defined) v) config.list-of-lists;
        list-of-nullable = map (v: if v == null then null else v.__defined) config.list-of-nullable;
        map = lib.mapAttrs (_: v: v.__defined) config.map;
        mutually-recursive = config.mutually-recursive.__defined;
        nested = config.nested.__defined;
        nested-or-null = if config.nested-or-null == null then null else config.nested-or-null.__defined;
        null-default = config.null-default;
        nullable = config.nullable;
        optional = config.optional;
        optional-or-null = config.optional-or-null;
        recursive = config.recursive;
        required = config.required;
        strings-or-list = if builtins.isList config.strings-or-list then config.strings-or-list else if builtins.isList config.strings-or-list then map (v: v.__defined) config.strings-or-list else config.strings-or-list;
        value-or-list = config.value-or-list;
        with-default = config.with-default;
      };
      internal = true;
      readOnly = true;
    };
    either = lib.mkOption {
      type = lib.types.oneOf [
        lib.types.str
        (lib.types.submodule ({ config, options, ... }:
        {
          options = {
            __defined = lib.mkOption {
              default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
                one = config.one;
                two = config.two;
              };
              internal = true;
              readOnly = true;
            };
            one = lib.mkOption {
              type = lib.types.str;
            };
            two = lib.mkOption {
              type = lib.types.str;
            };
          };
        }))
      ];
    };
    either-list = lib.mkOption {
      type = lib.types.oneOf [
        (lib.types.submodule ({ config, options, ... }:
        {
          options = {
            __defined = lib.mkOption {
              default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
                one = config.one;
                two = config.two;
              };
              internal = true;
              readOnly = true;
            };
            one = lib.mkOption {
              type = lib.types.str;
            };
            two = lib.mkOption {
              type = lib.types.str;
            };
          };
        }))
        (lib.types.listOf (lib.types.submodule ({ config, options, ... }:
        {
          options = {
            __defined = lib.mkOption {
              default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
                one = config.one;
                two = config.two;
              };
              internal = true;
              readOnly = true;
            };
            one = lib.mkOption {
              type = lib.types.str;
            };
            two = lib.mkOption {
              type = lib.types.str;
            };
          };
        })))
      ];
    };
    either-object = lib.mkOption {
      type = lib.types.submodule ({ config, options, ... }:
      {
        options = {
          __defined = lib.mkOption {
            default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
              a = config.a;
              b = config.b;
              mode = config.mode;
            };
            internal = true;
            readOnly = true;
          };
          a = lib.mkOption {
            type = lib.types.str;
          };
          b = lib.mkOption {
            type = lib.types.str;
          };
          mode = lib.mkOption {
            type = lib.types.oneOf [
              lib.types.str
              lib.types.int
            ];
          };
        };
      });
    };
    list = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule ({ config, options, ... }:
      {
        options = {
          __defined = lib.mkOption {
            default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
              one = config.one;
              two = config.two;
            };
            internal = true;
            readOnly = true;
          };
          one = lib.mkOption {
            type = lib.types.str;
          };
          two = lib.mkOption {
            type = lib.types.str;
          };
        };
      }));
    };
    list-of-lists = lib.mkOption {
      type = lib.types.listOf (lib.types.listOf (lib.types.submodule ({ config, options, ... }:
      {
        options = {
          __defined = lib.mkOption {
            default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
              one = config.one;
              two = config.two;
            };
            internal = true;
            readOnly = true;
          };
          one = lib.mkOption {
            type = lib.types.str;
          };
          two = lib.mkOption {
            type = lib.types.str;
          };
        };
      })));
    };
    list-of-nullable = lib.mkOption {
      type = lib.types.listOf (lib.types.nullOr (lib.types.submodule ({ config, options, ... }:
      {
        options = {
          __defined = lib.mkOption {
            default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
              one = config.one;
              two = config.two;
            };
            internal = true;
            readOnly = true;
          };
          one = lib.mkOption {
            type = lib.types.str;
          };
          two = lib.mkOption {
            type = lib.types.str;
          };
        };
      })));
    };
    map = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule ({ config, options, ... }:
      {
        options = {
          __defined = lib.mkOption {
            default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
              one = config.one;
              two = config.two;
            };
            internal = true;
            readOnly = true;
          };
          one = lib.mkOption {
            type = lib.types.str;
          };
          two = lib.mkOption {
            type = lib.types.str;
          };
        };
      }));
    };
    mutually-recursive = lib.mkOption {
      type = lib.types.submodule ({ config, options, ... }:
      {
        options = {
          __defined = lib.mkOption {
            default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
              relationshipToA = config.relationshipToA;
            };
            internal = true;
            readOnly = true;
          };
          relationshipToA = lib.mkOption { };
        };
      });
    };
    nested = lib.mkOption {
      type = lib.types.submodule ({ config, options, ... }:
      {
        options = {
          __defined = lib.mkOption {
            default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
              one = config.one;
              two = config.two;
            };
            internal = true;
            readOnly = true;
          };
          one = lib.mkOption {
            type = lib.types.str;
          };
          two = lib.mkOption {
            type = lib.types.str;
          };
        };
      });
    };
    nested-or-null = lib.mkOption {
      type = lib.types.nullOr (lib.types.submodule ({ config, options, ... }:
      {
        options = {
          __defined = lib.mkOption {
            default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
              one = config.one;
              two = config.two;
            };
            internal = true;
            readOnly = true;
          };
          one = lib.mkOption {
            type = lib.types.str;
          };
          two = lib.mkOption {
            type = lib.types.str;
          };
        };
      }));
    };
    null-default = lib.mkOption {
      default = null;
      type = lib.types.nullOr lib.types.str;
    };
    nullable = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
    };
    optional = lib.mkOption {
      type = lib.types.str;
    };
    optional-or-null = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
    };
    recursive = lib.mkOption { };
    required = lib.mkOption {
      type = lib.types.str;
    };
    strings-or-list = lib.mkOption {
      type = lib.types.oneOf [
        (lib.types.listOf lib.types.str)
        (lib.types.listOf (lib.types.submodule ({ config, options, ... }:
        {
          options = {
            __defined = lib.mkOption {
              default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
                one = config.one;
                two = config.two;
              };
              internal = true;
              readOnly = true;
            };
            one = lib.mkOption {
              type = lib.types.str;
            };
            two = lib.mkOption {
              type = lib.types.str;
            };
          };
        })))
      ];
    };
    value-or-list = lib.mkOption {
      type = lib.types.oneOf [
        lib.types.unspecified
        (lib.types.listOf (lib.types.submodule ({ config, options, ... }:
        {
          options = {
            __defined = lib.mkOption {
              default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
                one = config.one;
                two = config.two;
              };
              internal = true;
              readOnly = true;
            };
            one = lib.mkOption {
              type = lib.types.str;
            };
            two = lib.mkOption {
              type = lib.types.str;
            };
          };
        })))
      ];
    };
    with-default = lib.mkOption {
      default = "default";
      type = lib.types.str;
    };
  };
})
