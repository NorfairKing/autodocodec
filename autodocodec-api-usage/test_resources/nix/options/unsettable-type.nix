{ lib }:
({ definedSubmodule }:
definedSubmodule (lib.types.submodule ({ config, options, ... }:
{
  options = {
    "1st" = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.str;
    };
    __defined = lib.mkOption {
      default = (lib.mergeAttrs {
        nullable = if options.nullable.isDefined then config.nullable else throw "The option `${lib.showOption options.nullable.loc}' is required but was not defined.";
        required = if options.required.isDefined then config.required else throw "The option `${lib.showOption options.required.loc}' is required but was not defined.";
      }) ((lib.filterAttrs (name: _: options.${name}.isDefined)) {
        "1st" = config."1st";
        either = config.either;
        either-list = config.either-list;
        either-object = config.either-object;
        either-objects = config.either-objects;
        list = config.list;
        list-of-lists = config.list-of-lists;
        list-of-nullable = config.list-of-nullable;
        map = config.map;
        mutually-recursive = config.mutually-recursive;
        nested = config.nested;
        nested-or-null = config.nested-or-null;
        nested-required = config.nested-required;
        null-default = config.null-default;
        optional = config.optional;
        optional-or-null = config.optional-or-null;
        recursive = config.recursive;
        strings-or-list = config.strings-or-list;
        these = config.these;
        value-or-list = config.value-or-list;
        with-default = config.with-default;
      });
      internal = true;
      readOnly = true;
    };
    either = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.oneOf [
        lib.types.str
        (definedSubmodule (lib.types.submodule ({ config, options, ... }:
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
              defaultText = lib.literalMD "None, this option is left out when it is not set.";
              type = lib.types.str;
            };
            two = lib.mkOption {
              defaultText = lib.literalMD "None, this option is left out when it is not set.";
              type = lib.types.str;
            };
          };
        })))
      ];
    };
    either-list = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.oneOf [
        (definedSubmodule (lib.types.submodule ({ config, options, ... }:
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
              defaultText = lib.literalMD "None, this option is left out when it is not set.";
              type = lib.types.str;
            };
            two = lib.mkOption {
              defaultText = lib.literalMD "None, this option is left out when it is not set.";
              type = lib.types.str;
            };
          };
        })))
        (lib.types.listOf (definedSubmodule (lib.types.submodule ({ config, options, ... }:
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
              defaultText = lib.literalMD "None, this option is left out when it is not set.";
              type = lib.types.str;
            };
            two = lib.mkOption {
              defaultText = lib.literalMD "None, this option is left out when it is not set.";
              type = lib.types.str;
            };
          };
        }))))
      ];
    };
    either-object = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = definedSubmodule (lib.types.submodule ({ config, options, ... }:
      {
        options = {
          __defined = lib.mkOption {
            default = (lib.mergeAttrs {
              both = if options.both.isDefined then config.both else throw "The option `${lib.showOption options.both.loc}' is required but was not defined.";
            }) ((lib.filterAttrs (name: _: options.${name}.isDefined)) {
              a = config.a;
              b = config.b;
              mode = config.mode;
              only-a = config.only-a;
            });
            internal = true;
            readOnly = true;
          };
          a = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
          b = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
          both = lib.mkOption {
            defaultText = lib.literalMD "None, this option is required.";
            type = lib.types.str;
          };
          mode = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.oneOf [
              lib.types.str
              lib.types.int
            ];
          };
          only-a = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
        };
      }));
    };
    either-objects = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = definedSubmodule (lib.types.submodule ({ config, options, ... }:
      {
        options = {
          __defined = lib.mkOption {
            default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
              a = config.a;
              b = config.b;
            };
            internal = true;
            readOnly = true;
          };
          a = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
          b = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
        };
      }));
    };
    list = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.listOf (definedSubmodule (lib.types.submodule ({ config, options, ... }:
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
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
          two = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
        };
      })));
    };
    list-of-lists = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.listOf (lib.types.listOf (definedSubmodule (lib.types.submodule ({ config, options, ... }:
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
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
          two = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
        };
      }))));
    };
    list-of-nullable = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.listOf (lib.types.nullOr (definedSubmodule (lib.types.submodule ({ config, options, ... }:
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
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
          two = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
        };
      }))));
    };
    map = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.attrsOf (definedSubmodule (lib.types.submodule ({ config, options, ... }:
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
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
          two = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
        };
      })));
    };
    mutually-recursive = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = definedSubmodule (lib.types.submodule ({ config, options, ... }:
      {
        options = {
          __defined = lib.mkOption {
            default = (lib.filterAttrs (name: _: options.${name}.isDefined)) {
              relationshipToA = config.relationshipToA;
            };
            internal = true;
            readOnly = true;
          };
          relationshipToA = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
          };
        };
      }));
    };
    nested = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = definedSubmodule (lib.types.submodule ({ config, options, ... }:
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
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
          two = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
        };
      }));
    };
    nested-or-null = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.nullOr (definedSubmodule (lib.types.submodule ({ config, options, ... }:
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
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
          two = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
        };
      })));
    };
    nested-required = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = definedSubmodule (lib.types.submodule ({ config, options, ... }:
      {
        options = {
          __defined = lib.mkOption {
            default = (lib.mergeAttrs {
              one = if options.one.isDefined then config.one else throw "The option `${lib.showOption options.one.loc}' is required but was not defined.";
            }) ((lib.filterAttrs (name: _: options.${name}.isDefined)) {
              two = config.two;
            });
            internal = true;
            readOnly = true;
          };
          one = lib.mkOption {
            defaultText = lib.literalMD "None, this option is required.";
            type = lib.types.str;
          };
          two = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
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
      defaultText = lib.literalMD "None, this option is required.";
      type = lib.types.nullOr lib.types.str;
    };
    optional = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.str;
    };
    optional-or-null = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.nullOr lib.types.str;
    };
    recursive = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
    };
    required = lib.mkOption {
      defaultText = lib.literalMD "None, this option is required.";
      type = lib.types.str;
    };
    strings-or-list = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.oneOf [
        (lib.types.listOf lib.types.str)
        (lib.types.listOf (definedSubmodule (lib.types.submodule ({ config, options, ... }:
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
              defaultText = lib.literalMD "None, this option is left out when it is not set.";
              type = lib.types.str;
            };
            two = lib.mkOption {
              defaultText = lib.literalMD "None, this option is left out when it is not set.";
              type = lib.types.str;
            };
          };
        }))))
      ];
    };
    these = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = definedSubmodule (lib.types.submodule ({ config, options, ... }:
      {
        options = {
          __defined = lib.mkOption {
            default = (lib.mergeAttrs {
              type = if options.type.isDefined then config.type else throw "The option `${lib.showOption options.type.loc}' is required but was not defined.";
            }) ((lib.filterAttrs (name: _: options.${name}.isDefined)) {
              int = config.int;
              text = config.text;
            });
            internal = true;
            readOnly = true;
          };
          int = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.int;
          };
          text = lib.mkOption {
            defaultText = lib.literalMD "None, this option is left out when it is not set.";
            type = lib.types.str;
          };
          type = lib.mkOption {
            defaultText = lib.literalMD "None, this option is required.";
            type = lib.types.enum [
              "both"
              "this"
              "that"
            ];
          };
        };
      }));
    };
    value-or-list = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      type = lib.types.oneOf [
        lib.types.unspecified
        (lib.types.listOf (definedSubmodule (lib.types.submodule ({ config, options, ... }:
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
              defaultText = lib.literalMD "None, this option is left out when it is not set.";
              type = lib.types.str;
            };
            two = lib.mkOption {
              defaultText = lib.literalMD "None, this option is left out when it is not set.";
              type = lib.types.str;
            };
          };
        }))))
      ];
    };
    with-default = lib.mkOption {
      default = "default";
      type = lib.types.str;
    };
  };
}))) {
  definedSubmodule = let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue = if submodule.emptyValue ? value then { value = submodule.emptyValue.value.__defined; } else submodule.emptyValue; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule;
}
