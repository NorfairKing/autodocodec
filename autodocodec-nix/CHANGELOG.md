# Changelog

## [0.2.0.0] - 2026-11-14

### Added

* `ExprLam`, `ExprLet`, `ExprIf`, `ExprUpdate` and `ExprSelect` constructors
  of `Expression`, for a function with a single argument, `let`, `if`, `//`
  and selecting an attribute.

### Changed

* A function, `with`, `let` or `if` is put in parentheses wherever it is not
  the last part of an expression, and an application of an application is no
  longer put in parentheses.

### Fixed

* A function that takes up more than one line is separated from the argument
  it is applied to by a space.

## [0.1.1.1] - 2026-11-14

### Fixed

* Strings and quoted attribute keys are rendered as Nix string literals.
  They used to be rendered as Haskell string literals, so Nix read a
  non-ASCII character like `é` as its code point, `233`, and interpolated a
  `${`.
* A negative number in a list or as an argument is put in parentheses.
  Nix read `[ -1 ]` as a subtraction, which it could not parse.
* The element of a list of one element is put in parentheses when it needs
  them, so an application in it is no longer read as two elements.

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
