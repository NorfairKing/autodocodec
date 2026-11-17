{ lib }:
lib.mkOption {
  defaultText = lib.literalMD "Unspecified";
  example = {
    host = "localhost";
    port = 8000;
  };
}
