{ lib }:
let
  definedSubmodule = required: declarations: definedType (lib.types.submodule ({ config, options, ... }:
  let
    configValue = name: lib.getAttr name config;
    isDefined = name: (lib.getAttr name options).isDefined;
    requiredValue = name: if isDefined name then configValue name else throw (lib.concatStrings [
      "The option `"
      (lib.showOption (lib.getAttr name options).loc)
      "' is required but was not defined."
    ]);
  in
  {
    options = lib.throwIf (lib.hasAttr "__defined" declarations) "autodocodec-nix uses the option name __defined itself." declarations // {
      __defined = lib.mkOption {
        default = lib.genAttrs required requiredValue // lib.genAttrs (lib.filter isDefined (lib.attrNames declarations)) configValue;
        internal = true;
        readOnly = true;
      };
    };
  }));
  definedType = submodule: submodule // {
    emptyValue = if lib.hasAttr "value" submodule.emptyValue then {
      value = submodule.emptyValue.value.__defined;
    } else submodule.emptyValue;
    functor = submodule.functor // {
      name = "definedSubmodule";
    };
    merge = if lib.isAttrs submodule.merge then {
      __functor = self: loc: defs: (self.v2 {
        defs = defs;
        loc = loc;
      }).value;
      v2 = args: let
        merged = submodule.merge.v2 args;
      in
      merged // {
        value = merged.value.__defined;
      };
    } else loc: defs: (submodule.merge loc defs).__defined;
    substSubModules = modules: definedType (submodule.substSubModules modules);
    typeMerge = _: null;
  };
in
definedSubmodule [
  "nullable"
  "required"
] {
  "1st" = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = lib.types.str;
  };
  either = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = lib.types.oneOf [
      lib.types.str
      (definedSubmodule [] {
        one = lib.mkOption {
          defaultText = lib.literalMD "None, this option is left out when it is not set.";
          type = lib.types.str;
        };
        two = lib.mkOption {
          defaultText = lib.literalMD "None, this option is left out when it is not set.";
          type = lib.types.str;
        };
      })
    ];
  };
  either-list = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = lib.types.oneOf [
      (definedSubmodule [] {
        one = lib.mkOption {
          defaultText = lib.literalMD "None, this option is left out when it is not set.";
          type = lib.types.str;
        };
        two = lib.mkOption {
          defaultText = lib.literalMD "None, this option is left out when it is not set.";
          type = lib.types.str;
        };
      })
      (lib.types.listOf (definedSubmodule [] {
        one = lib.mkOption {
          defaultText = lib.literalMD "None, this option is left out when it is not set.";
          type = lib.types.str;
        };
        two = lib.mkOption {
          defaultText = lib.literalMD "None, this option is left out when it is not set.";
          type = lib.types.str;
        };
      }))
    ];
  };
  either-object = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = definedSubmodule ["both"] {
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
  };
  either-objects = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = definedSubmodule [] {
      a = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
        type = lib.types.str;
      };
      b = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
        type = lib.types.str;
      };
    };
  };
  list = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = lib.types.listOf (definedSubmodule [] {
      one = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
        type = lib.types.str;
      };
      two = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
        type = lib.types.str;
      };
    });
  };
  list-of-lists = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = lib.types.listOf (lib.types.listOf (definedSubmodule [] {
      one = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
        type = lib.types.str;
      };
      two = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
        type = lib.types.str;
      };
    }));
  };
  list-of-nullable = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = lib.types.listOf (lib.types.nullOr (definedSubmodule [] {
      one = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
        type = lib.types.str;
      };
      two = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
        type = lib.types.str;
      };
    }));
  };
  map = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = lib.types.attrsOf (definedSubmodule [] {
      one = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
        type = lib.types.str;
      };
      two = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
        type = lib.types.str;
      };
    });
  };
  mutually-recursive = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = definedSubmodule [] {
      relationshipToA = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
      };
    };
  };
  nested = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = definedSubmodule [] {
      one = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
        type = lib.types.str;
      };
      two = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
        type = lib.types.str;
      };
    };
  };
  nested-or-null = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = lib.types.nullOr (definedSubmodule [] {
      one = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
        type = lib.types.str;
      };
      two = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
        type = lib.types.str;
      };
    });
  };
  nested-required = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = definedSubmodule ["one"] {
      one = lib.mkOption {
        defaultText = lib.literalMD "None, this option is required.";
        type = lib.types.str;
      };
      two = lib.mkOption {
        defaultText = lib.literalMD "None, this option is left out when it is not set.";
        type = lib.types.str;
      };
    };
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
      (lib.types.listOf (definedSubmodule [] {
        one = lib.mkOption {
          defaultText = lib.literalMD "None, this option is left out when it is not set.";
          type = lib.types.str;
        };
        two = lib.mkOption {
          defaultText = lib.literalMD "None, this option is left out when it is not set.";
          type = lib.types.str;
        };
      }))
    ];
  };
  these = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = definedSubmodule ["type"] {
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
  };
  value-or-list = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    type = lib.types.oneOf [
      lib.types.unspecified
      (lib.types.listOf (definedSubmodule [] {
        one = lib.mkOption {
          defaultText = lib.literalMD "None, this option is left out when it is not set.";
          type = lib.types.str;
        };
        two = lib.mkOption {
          defaultText = lib.literalMD "None, this option is left out when it is not set.";
          type = lib.types.str;
        };
      }))
    ];
  };
  with-default = lib.mkOption {
    default = "default";
    type = lib.types.str;
  };
}
