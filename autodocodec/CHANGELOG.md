# Changelog

## [0.2.0.5] - 2023-08-27

### Added

* JSON Object-specific versions of encoding and decoding functions
* Documentation about how 'parseAlternative' and 'optionalField' together can be a pitfall.

## [0.2.0.4] - 2023-07-31

### Added

* The `HasCodec Identity` instance, thanks @clintonmead!

## [0.2.0.3] - 2023-01-31

### Added

* The `HasCodec Void` instance, thanks @i-am-tom!

## [0.2.0.2] - 2023-01-19

### Changed

* Compatibility with `mtl-2.3.1`

## [0.2.0.1] - 2022-10-06

### Added

* The `HasObjectCodec` type class

## [0.2.0.0] - 2022-07-21

### Added

* `discriminatedUnionCodec` for discriminated unions

## [0.1.0.3] - 2022-07-14

### Changed

* Doctest fix

## [0.1.0.2] - 2022-06-24

### Added

* `scientificWithBoundsCodec` for a `NumberCodec` with bounds but without a name.

## [0.1.0.1] - 2022-05-03

### Changed

* Generalised the type of `parseAlternative`.

## [0.1.0.0] - 2022-05-03

### Changed

* Generalise type of `matchChoiceCodec` to allow for two different input types.
* Add disjoint versions of `matchChoiceCodec` and `matchChoicesCodec`.
* Functions `enumCodec`, `stringConstCodec`, and `shownBoundedEnumCodec` now produce disjoint codecs.

## [0.0.1.1] - 2022-04-26

### Added

* Compatibility with `aeson >= 2.0.0.0`

## [0.0.1.0] - 2021-12-23

### Changed

* `EitherCodec` now takes a `Union` to specify whether the union is disjoint or not.

### Added

* `disjointEitherCodec` and `possiblyJointEitherCodec`.

## [0.0.0.0] - 2021-11-19

First release.
