{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Autodocodec
  ( -- * Encoding and decoding JSON
    encodeJSONViaCodec,
    eitherDecodeJSONViaCodec,

    -- * Instantiating 'Aeson.ToJSON'
    toJSONViaCodec,
    toJSONVia,
    toEncodingViaCodec,
    toEncodingVia,

    -- * Instantiating 'Aeson.FromJSON'
    parseJSONViaCodec,
    parseJSONVia,

    -- * Codec
    JSONCodec,
    JSONObjectCodec,
    HasCodec (..),

    -- * Writing a codec
    object,
    named,
    codecViaAeson,

    -- ** Field codecs

    -- *** With documentation
    requiredField,
    optionalField,
    (.=),
    optionalFieldOrNull,
    optionalFieldWithDefault,
    requiredFieldWith,
    optionalFieldWith,
    optionalFieldOrNullWith,
    optionalFieldWithDefaultWith,
    optionalFieldWithOmittedDefault,
    optionalFieldWithOmittedDefaultWith,
    optionalFieldOrNullWithOmittedDefault,
    optionalFieldOrNullWithOmittedDefaultWith,

    -- *** Documentation-less versions of field codecs
    requiredField',
    optionalField',
    optionalFieldOrNull',
    optionalFieldWithDefault',
    requiredFieldWith',
    optionalFieldWith',
    optionalFieldOrNullWith',
    optionalFieldWithDefaultWith',
    optionalFieldWithOmittedDefault',
    optionalFieldWithOmittedDefaultWith',
    optionalFieldOrNullWithOmittedDefault',
    optionalFieldOrNullWithOmittedDefaultWith',

    -- ** Writing your own value codecs.

    -- *** Primitive codecs
    nullCodec,
    boolCodec,
    textCodec,
    stringCodec,
    scientificCodec,
    scientificWithBoundsCodec,
    valueCodec,

    -- *** Integral codecs
    boundedIntegralCodec,
    boundedIntegralNumberBounds,

    -- *** Literal value codecs
    literalTextCodec,
    literalTextValueCodec,

    -- *** Enums
    shownBoundedEnumCodec,
    stringConstCodec,
    enumCodec,

    -- *** Sum type codecs
    eitherCodec,
    disjointEitherCodec,
    possiblyJointEitherCodec,

    -- *** Mapping
    dimapCodec,
    bimapCodec,
    rmapCodec,
    lmapCodec,

    -- *** Composing codecs
    maybeCodec,
    listCodec,
    nonEmptyCodec,
    singleOrListCodec,
    singleOrNonEmptyCodec,
    vectorCodec,

    -- *** Alternative parsing
    parseAlternative,
    parseAlternatives,

    -- *** Choice
    matchChoiceCodec,
    disjointMatchChoiceCodec,
    matchChoiceCodecAs,
    matchChoicesCodec,
    disjointMatchChoicesCodec,
    matchChoicesCodecAs,

    -- *** Adding documentation to a codec
    (<?>),
    (<??>),

    -- * Bare codec
    Codec (..),
    ValueCodec,
    ObjectCodec,
    pureCodec,
    apCodec,

    -- * Deriving Via
    Autodocodec (..),

    -- ** Internals you most likely don't need
    showCodecABit,

    -- * To make sure we definitely export everything
    module Autodocodec.Aeson,
    module Autodocodec.Class,
    module Autodocodec.DerivingVia,
    module Autodocodec.Codec,
  )
where

import Autodocodec.Aeson
import Autodocodec.Class
import Autodocodec.Codec
import Autodocodec.DerivingVia
