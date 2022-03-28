# Changelog

## [0.2.0.0] - 2022-04-05

### Changed
* Fixed an issue where, when using `named` and mutually recursive types, not all schemas would be declared when the top level type was declared with `declareSchemaRef` from openapi3 (#16)
* Fixed an issue where using `named` would declare a named schema, but would return an un-named schema, sometimes leading to duplicate schema definitions (#16)
* `declareSpecificNamedSchemaRef` and `declareSpecificSchemaRef` now work with any `MonadDeclare`, not just the `Declare` concrete monad (#16)

## [0.1.0.0] - 2021-12-23

### Added

* `disjointEitherCodec` now no longer generates `additionalProperties = true` and uses `oneOf` instead of `anyOf`.

### Changed

* Now uses `nullable: true` instead of `anyOf` for maybe codecs.

## [0.0.0.0] - 2021-11-19

First release.
