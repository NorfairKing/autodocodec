{ lib }:
let
  definedSubmodule = required: declarations: definedType required (lib.types.submodule {
    options = declarations;
  });
  definedType = required: submodule: submodule // {
    emptyValue = { };
    functor = submodule.functor // {
      name = "definedSubmodule";
    };
    merge = submodule.merge // {
      v2 = args: let
        configValue = name: lib.getAttr name merged.value;
        isDefined = name: (lib.getAttr name options).isDefined;
        merged = submodule.merge.v2 args;
        options = merged.valueMeta.configuration.options;
        requiredValue = name: if isDefined name then configValue name else throw (lib.concatStrings [
          "The option `"
          (lib.showOption (lib.getAttr name options).loc)
          "' is required but was not defined."
        ]);
      in
      merged // {
        value = lib.genAttrs required requiredValue // lib.genAttrs (lib.filter isDefined (lib.attrNames (lib.removeAttrs options ["_module"]))) configValue;
      };
    };
    substSubModules = modules: definedType required (submodule.substSubModules modules);
    typeMerge = _: null;
  };
in
definedSubmodule [
  "nullable"
  "required"
] {
  "1st" = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    type = lib.types.str;
  };
  either = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    type = lib.types.oneOf [
      lib.types.str
      (definedSubmodule [] {
        one = lib.mkOption {
          defaultText = lib.literalMD "Unspecified";
          type = lib.types.str;
        };
        two = lib.mkOption {
          defaultText = lib.literalMD "Unspecified";
          type = lib.types.str;
        };
      })
    ];
  };
  either-list = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    type = lib.types.oneOf [
      (definedSubmodule [] {
        one = lib.mkOption {
          defaultText = lib.literalMD "Unspecified";
          type = lib.types.str;
        };
        two = lib.mkOption {
          defaultText = lib.literalMD "Unspecified";
          type = lib.types.str;
        };
      })
      (lib.types.listOf (definedSubmodule [] {
        one = lib.mkOption {
          defaultText = lib.literalMD "Unspecified";
          type = lib.types.str;
        };
        two = lib.mkOption {
          defaultText = lib.literalMD "Unspecified";
          type = lib.types.str;
        };
      }))
    ];
  };
  either-object = lib.mkOption {
    type = definedSubmodule ["both"] {
      a = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
      b = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
      both = lib.mkOption {
        defaultText = lib.literalMD "None, required" // {
          required = true;
        };
        type = lib.types.str;
      };
      level = lib.mkOption {
        defaultText = lib.literalMD "`1` or `2`, depending on the first alternative that parses";
        type = lib.types.int;
      };
      mode = lib.mkOption {
        defaultText = lib.literalMD "`\"a\"` or unspecified, depending on the first alternative that parses";
        type = lib.types.oneOf [
          lib.types.str
          lib.types.int
        ];
      };
      only-a = lib.mkOption {
        defaultText = lib.literalMD "`\"x\"` or unspecified, depending on the first alternative that parses";
        type = lib.types.str;
      };
    };
  };
  either-objects = lib.mkOption {
    type = definedSubmodule [] {
      a = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
      b = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
    };
  };
  list = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    type = lib.types.listOf (definedSubmodule [] {
      one = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
      two = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
    });
  };
  list-of-lists = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    type = lib.types.listOf (lib.types.listOf (definedSubmodule [] {
      one = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
      two = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
    }));
  };
  list-of-nullable = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    type = lib.types.listOf (lib.types.nullOr (definedSubmodule [] {
      one = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
      two = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
    }));
  };
  map = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    type = lib.types.attrsOf (definedSubmodule [] {
      one = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
      two = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
    });
  };
  mutually-recursive = lib.mkOption {
    type = definedSubmodule [] {
      relationshipToA = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
      };
    };
  };
  nested = lib.mkOption {
    type = definedSubmodule [] {
      one = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
      two = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
    };
  };
  nested-or-null = lib.mkOption {
    type = lib.types.nullOr (definedSubmodule [] {
      one = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
      two = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
    });
  };
  nested-required = lib.mkOption {
    type = definedSubmodule ["one"] {
      one = lib.mkOption {
        defaultText = lib.literalMD "None, required" // {
          required = true;
        };
        type = lib.types.str;
      };
      two = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
    };
  };
  null-default = lib.mkOption {
    default = null;
    type = lib.types.nullOr lib.types.str;
  };
  nullable = lib.mkOption {
    defaultText = lib.literalMD "None, required" // {
      required = true;
    };
    type = lib.types.nullOr lib.types.str;
  };
  optional = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    type = lib.types.str;
  };
  optional-or-null = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    type = lib.types.nullOr lib.types.str;
  };
  recursive = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
  };
  required = lib.mkOption {
    defaultText = lib.literalMD "None, required" // {
      required = true;
    };
    type = lib.types.str;
  };
  strings-or-list = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    type = lib.types.oneOf [
      (lib.types.listOf lib.types.str)
      (lib.types.listOf (definedSubmodule [] {
        one = lib.mkOption {
          defaultText = lib.literalMD "Unspecified";
          type = lib.types.str;
        };
        two = lib.mkOption {
          defaultText = lib.literalMD "Unspecified";
          type = lib.types.str;
        };
      }))
    ];
  };
  these = lib.mkOption {
    type = definedSubmodule ["type"] {
      int = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.int;
      };
      text = lib.mkOption {
        defaultText = lib.literalMD "Unspecified";
        type = lib.types.str;
      };
      type = lib.mkOption {
        defaultText = lib.literalMD "None, required" // {
          required = true;
        };
        type = lib.types.enum [
          "both"
          "that"
          "this"
        ];
      };
    };
  };
  value-or-list = lib.mkOption {
    defaultText = lib.literalMD "Unspecified";
    type = lib.types.oneOf [
      lib.types.unspecified
      (lib.types.listOf (definedSubmodule [] {
        one = lib.mkOption {
          defaultText = lib.literalMD "Unspecified";
          type = lib.types.str;
        };
        two = lib.mkOption {
          defaultText = lib.literalMD "Unspecified";
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
