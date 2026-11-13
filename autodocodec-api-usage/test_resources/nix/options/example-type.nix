{ lib }:
({ definedSubmodule }:
definedSubmodule (lib.types.submodule ({ config, options, ... }:
{
  options = {
    __defined = lib.mkOption {
      default = lib.mergeAttrs {
        bool = if options.bool.isDefined then config.bool else throw "The option `${lib.showOption options.bool.loc}' is required but was not defined.";
        fruit = if options.fruit.isDefined then config.fruit else throw "The option `${lib.showOption options.fruit.loc}' is required but was not defined.";
        maybe = if options.maybe.isDefined then config.maybe else throw "The option `${lib.showOption options.maybe.loc}' is required but was not defined.";
        shape = if options.shape.isDefined then config.shape else throw "The option `${lib.showOption options.shape.loc}' is required but was not defined.";
        text = if options.text.isDefined then config.text else throw "The option `${lib.showOption options.text.loc}' is required but was not defined.";
      } (lib.filterAttrs (name: _: options.${name}.isDefined) {
        optional = config.optional;
        optional-or-null = config.optional-or-null;
        optional-with-default = config.optional-with-default;
        optional-with-null-default = config.optional-with-null-default;
        single-or-list = config.single-or-list;
      });
      internal = true;
      readOnly = true;
    };
    bool = lib.mkOption {
      defaultText = lib.literalMD "None, this option is required.";
      description = "a bool";
      type = lib.types.bool;
    };
    fruit = lib.mkOption {
      defaultText = lib.literalMD "None, this option is required.";
      description = "fruit!!";
      type = lib.types.enum [
        "Apple"
        "Orange"
        "Banana"
        "Melon"
      ];
    };
    maybe = lib.mkOption {
      defaultText = lib.literalMD "None, this option is required.";
      description = "a maybe text";
      type = lib.types.nullOr lib.types.str;
    };
    optional = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      description = "an optional text";
      type = lib.types.str;
    };
    optional-or-null = lib.mkOption {
      defaultText = lib.literalMD "None, this option is left out when it is not set.";
      description = "an optional-or-null text";
      type = lib.types.nullOr lib.types.str;
    };
    optional-with-default = lib.mkOption {
      default = "foobar";
      description = "an optional text with a default";
      type = lib.types.str;
    };
    optional-with-null-default = lib.mkOption {
      default = [];
      description = "an optional list of texts with a default empty list where the empty list would be omitted";
      type = lib.types.listOf lib.types.str;
    };
    shape = lib.mkOption {
      defaultText = lib.literalMD "None, this option is required.";
      description = "shape!?";
      type = lib.types.enum [
        "circle"
        "square"
        "rectangle"
      ];
    };
    single-or-list = lib.mkOption {
      default = [];
      description = "an optional list that can also be specified as a single element";
      type = lib.types.oneOf [
        lib.types.str
        (lib.types.listOf lib.types.str)
      ];
    };
    text = lib.mkOption {
      defaultText = lib.literalMD "None, this option is required.";
      description = "a text";
      type = lib.types.str;
    };
  };
}))) {
  definedSubmodule = let definedSubmodule = submodule: submodule // { merge = if submodule.merge ? v2 then { __functor = self: loc: defs: (self.v2 { inherit loc defs; }).value; v2 = args: let merged = submodule.merge.v2 args; in merged // { value = merged.value.__defined; }; } else loc: defs: (submodule.merge loc defs).__defined; emptyValue = if submodule.emptyValue ? value then { value = submodule.emptyValue.value.__defined; } else submodule.emptyValue; substSubModules = modules: definedSubmodule (submodule.substSubModules modules); }; in definedSubmodule;
}
