# Changelog

## [0.1.0.0] - 2025-02-13

### Added

* The `Autodocodec.Nix.Render` module for rendering values as Nix expressions.

### Changed

* Moved the options code into `Autodocodec.Nix.Options`.
* Moved the expressions code into `Autodocodec.Nix.Expression`.

## [0.0.1.5] - 2024-11-04

### Changed

* More accurate support for `EitherCodec` in `ObjectCodec`s.
  Options' types in an object are now or-ed.

## [0.0.1.4] - 2024-08-22

### Changed

* More accurate support for `EitherCodec` in `ObjectCodec`s.

## [0.0.1.3] - 2024-08-22

### Changed

* Made required object codecs produce a required type.

## [0.0.1.2] - 2024-08-21

### Changed

* Fixed the nix type that corresponds to `null`.
* Fixed that some nix types were not being simplified enough.

## [0.0.1.1] - 2024-08-20

### Changed

* Fixed the nix type that corresponds to `Word`.

## [0.0.1.0] - 2024-07-31

### Changed

* More precise types for `EqCodec` codecs like enums.

## [0.0.0.0] - 2024-07-19

Initial version
