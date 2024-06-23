# Changelog

## [0.3.0.0] - 2024-06-23

### Added

* `jsonSchemaChunkLines`

### Changed

* Show number bounds in a nicer way.

## [0.2.0.3] - 2023-01-01

### Changed

* Use the `ToYaml` instance instead of the `ToJSON` instance when encoding to Yaml.
  This preserves the key order where `ToJSON` wouldn't.

## [0.2.0.2] - 2022-07-21

### Added

* Support for the `discriminatedUnionCodec` for discriminated unions in `autodocodec-0.2.0.0`

## [0.2.0.1] - 2022-04-28

### Changed

* Changed `renderPlainSchemaVia`, `renderPlainSchemaViaCodec`, `renderColouredSchemaVia` and `renderColouredSchemaViaCodec` to output `Text` instead of `ByteString`.

## [0.1.0.1] - 2022-04-26

### Added

* Compatibility with `aeson >= 2.0.0.0`

## [0.1.0.0] - 2021-12-23

### Changed

* Support for `autodocodec-schema >=0.1.0.0` with comments for `anyOf` and `oneOf`.
* Added special support for 'or null'.

## [0.0.0.0] - 2021-11-19

First release.
