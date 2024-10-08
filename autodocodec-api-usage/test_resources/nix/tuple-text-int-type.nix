{ lib }:
lib.types.listOf (lib.types.oneOf [
  lib.types.str
  lib.types.int
])
