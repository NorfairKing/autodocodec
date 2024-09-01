# Changelog

## [0.4.2.2] - 2024-09-01

### Added

* `boundedEnumCodec`

## [0.4.2.1] - 2024-08-21

### Added

* `optionalFieldOrNullWithDefault`
* `optionalFieldOrNullWithDefaultWith`
* `optionalFieldOrNullWithDefault'`
* `optionalFieldOrNullWithDefaultWith'`

## [0.4.2.0] - 2024-08-06

### Added

* `HasCodec` instances for `DList` and `DNonEmpty` from the `dlist` package.

## [0.4.1.0] - 2024-08-03

### Changed

* Moved the `ToJSON (Autodocodec a)` and `FromJSON (Autodocodec a)` instances so they are no longer orphans.

## [0.4.0.0] - 2024-07-26

This is technically a breaking change but it's unlikely that you'll need to
change any of your codecs.

### Changed

* Changed `NumberBounds` to `Bounds` and made both bounds optional.
* Added `IntegerCodec` as distinct from `NumberCodec` to represent integers
  effectively in documentation.

## [0.3.0.0] - 2024-07-19

### Added

* `HasCodec` instances for:
    * `Const`
    * `Dual`
    * `Semigroup.First`
    * `Semigroup.Last`
    * `Monoid.First`
    * `Monoid.Last`


### Changed

* Refactored `Codec` so it's input and output parameters have a representative, not nominal role.
  This means one can now use `deriving newtype` with the `HasCodec` class.
* Fixed infinitely looping `Identity` instance
* Improve the documentation of `time`-related codecs to show `<string>` instead of `<any>`.

## [0.2.3.0] - 2024-06-23

### Added

* `Ord NumberBounds` instance

## [0.2.2.0] - 2023-11-20

### Added

* `HasCodec Integer` and `HasCodec Natural` instances.

## [0.2.1.0] - 2023-10-06

### Added

* `HasCodec a => HasCodec (Vector a)` instance.

## [0.2.0.6] - 2023-10-05

### Added

* Forward-compatibility with `aeson >= 2.2`.

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
