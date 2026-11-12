{ lib }:
(let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
{
  options = {
    "1st" = lib.mkOption {
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
        value-or-list = config.value-or-list;
        with-default = config.with-default;
      });
      internal = true;
      readOnly = true;
    };
    either = lib.mkOption {
      type = lib.types.oneOf [
        lib.types.str
        ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
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
    either-list = lib.mkOption {
      type = lib.types.oneOf [
        ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
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
        (lib.types.listOf ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
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
        }))))
      ];
    };
    either-object = lib.mkOption {
      type = (let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
      {
        options = {
          __defined = lib.mkOption {
            default = (lib.mergeAttrs {
              both = if options.both.isDefined then config.both else throw "The option `${lib.showOption options.both.loc}' is required but was not defined.";
            }) ((lib.filterAttrs (name: _: options.${name}.isDefined)) {
              a = config.a;
              b = config.b;
              mode = config.mode;
            });
            internal = true;
            readOnly = true;
          };
          a = lib.mkOption {
            type = lib.types.str;
          };
          b = lib.mkOption {
            type = lib.types.str;
          };
          both = lib.mkOption {
            type = lib.types.str;
          };
          mode = lib.mkOption {
            type = lib.types.oneOf [
              lib.types.str
              lib.types.int
            ];
          };
        };
      }));
    };
    list = lib.mkOption {
      type = lib.types.listOf ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
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
    list-of-lists = lib.mkOption {
      type = lib.types.listOf (lib.types.listOf ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
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
      }))));
    };
    list-of-nullable = lib.mkOption {
      type = lib.types.listOf (lib.types.nullOr ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
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
      }))));
    };
    map = lib.mkOption {
      type = lib.types.attrsOf ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
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
    mutually-recursive = lib.mkOption {
      type = (let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
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
      }));
    };
    nested = lib.mkOption {
      type = (let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
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
    nested-or-null = lib.mkOption {
      type = lib.types.nullOr ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
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
    nested-required = lib.mkOption {
      type = (let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
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
        (lib.types.listOf ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
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
        }))))
      ];
    };
    value-or-list = lib.mkOption {
      type = lib.types.oneOf [
        lib.types.unspecified
        (lib.types.listOf ((let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue.value = submodule.emptyValue.value.__defined; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule) (lib.types.submodule ({ config, options, ... }:
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
        }))))
      ];
    };
    with-default = lib.mkOption {
      default = "default";
      type = lib.types.str;
    };
  };
}))
