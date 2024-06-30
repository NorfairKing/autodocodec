mkOption {
  type = attrsOf (types.submodule { options = {}; ));
  description = "These";
}
