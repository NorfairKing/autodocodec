# Changelog

## [0.2.0.0] - 2026-11-11

### Changed

* A key that may be left out gets an option without a default, instead of one
  that defaults to `null`, so that leaving it out and setting it to `null` can
  be told apart.
  Its type no longer admits `null` unless its codec does.
  An option only has a default when the codec has one, so it is `null` only
  when the codec's default is.
  A key in more than one alternative of an object only keeps its default when
  every alternative has the same one.
  Reading an option that was not defined throws, so every generated submodule
  has a read-only `__defined` option that holds only the options that were
  defined, which is what to hand to the codec.
  A generated submodule is therefore a function of `config` and `options`.
* A submodule option no longer gets a default of `{ }`, because the codec has
  no such default.

### Removed

* `renderNixOptionsVia`, `renderNixOptionsViaCodec`, `optionsExpression` and
  `optionsExpr`.
  A set of options has no `__defined` of its own, so it cannot be written out
  whole any more.
  Use the submodule type from `renderNixOptionTypeVia` instead.

### Added

* `nixAttrKey` in `Autodocodec.Nix.Expression`.

## [0.1.1.0] - 2026-10-06

### Changed

* A comment on a field's codec, from `<?>` or `<??>`, now ends up in the
  generated option's `description` after the field's own description.
  It used to be dropped, so documentation written on a shared type's codec
  never reached the NixOS module generated from it.
  This includes a field built with `optionalFieldOrNullWith`, whose codec sits
  under the `EitherCodec` that `maybeCodec` is.

## [0.1.0.2] - 2026-07-14

### Changed

* Lower bound on `autodocodec >=0.6`

## [0.1.0.1] - 2026-04-25

### Changed

* Attribute keys that are not valid Nix identifiers are now quoted in rendered expressions.

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
