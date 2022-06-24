# Changelog

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
