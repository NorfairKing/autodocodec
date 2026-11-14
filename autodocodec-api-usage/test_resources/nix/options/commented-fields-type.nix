{ lib }:
let
  definedSubmodule = required: declarations: lib.throwIf (lib.hasAttr "__defined" declarations) "autodocodec-nix uses the option name __defined itself." (definedType (lib.types.submodule ({ config, options, ... }:
  {
    options = declarations // {
      __defined = lib.mkOption {
        default = lib.genAttrs required (name: if (lib.getAttr name options).isDefined then lib.getAttr name config else throw (lib.concatStrings [
          "The option `"
          (lib.showOption (lib.getAttr name options).loc)
          "' is required but was not defined."
        ])) // lib.mapAttrs (name: _: lib.getAttr name config) (lib.filterAttrs (name: _: (lib.getAttr name options).isDefined) declarations);
        internal = true;
        readOnly = true;
      };
    };
  })));
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
  "required"
  "undescribed"
] {
  defaulted = lib.mkOption {
    default = "default";
    description = "the defaulted field\nmore about the defaulted field";
    type = lib.types.str;
  };
  optional = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    description = "the optional field\nmore about the optional field\non a second line";
    type = lib.types.str;
  };
  or-null = lib.mkOption {
    defaultText = lib.literalMD "None, this option is left out when it is not set.";
    description = "the or-null field\nmore about the or-null field";
    type = lib.types.nullOr lib.types.str;
  };
  required = lib.mkOption {
    defaultText = lib.literalMD "None, this option is required.";
    description = "the required field\nmore about the required field";
    type = lib.types.str;
  };
  undescribed = lib.mkOption {
    defaultText = lib.literalMD "None, this option is required.";
    description = "all this field has to say";
    type = lib.types.str;
  };
}
