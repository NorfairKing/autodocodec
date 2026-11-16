# Changelog

## Unreleased

### Added

* Support for `ExtraDocsCodec`: titles, formats, examples, `multipleOf` and
  `minItems` are added to the generated schemas, as well as descriptions.

### Fixed

* The documentation of an object field whose type has a named schema is no
  longer dropped.
  Such a field's schema is a `$ref` to a definition that every use of the type
  shares, and Swagger 2 ignores anything next to a `$ref`, so it is put on an
  `allOf` around the `$ref` instead: `{"allOf": [{"$ref": ...}], "description": ...}`.
  Fields without documentation, and fields of types without a name, are
  documented as before.

## [0.1.0.1] - 2026-07-14

### Changed

* Lower bound on `autodocodec >=0.6`

## [0.1.0.0] - 2024-07-29

* Dropped support for orphan instances.

## [0.0.1.4] - 2024-07-29

* Non-orphan version of the `ToSchema` deriving.

## [0.0.1.3] - 2024-07-26

* Support for `autodocodec >=0.4`.

## [0.0.1.1] - 2022-07-21

### Added

* Support for the `discriminatedUnionCodec` for discriminated unions in `autodocodec-0.2.0.0`

## [0.0.1.0] - 2021-11-19

### Added

* `disjointEitherCodec` now does not generate `additionalProperties = true`.

## [0.0.0.0] - 2021-11-19

First release.
