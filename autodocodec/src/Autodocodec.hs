{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Autodocodec
  ( -- * Encoding
    encodeViaCodec,
    toJSONViaCodec,
    toJSONVia,
    toEncodingViaCodec,
    toEncodingVia,

    -- * Decoding
    eitherDecodeViaCodec,
    parseJSONViaCodec,
    parseJSONVia,

    -- * Codec
    JSONCodec,
    HasCodec (..),

    -- * Writing a codec
    object,
    named,

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

    -- ** Writing your own value codecs.
    maybeCodec,
    eitherCodec,
    arrayCodec,
    listCodec,
    valueCodec,
    nullCodec,
    boolCodec,
    textCodec,
    stringCodec,
    scientificCodec,
    boundedIntegerCodec,
    literalText,
    literalTextValue,
    (<?>),
    (<??>),

    -- *** Mapping
    dimapCodec,
    bimapCodec,
    rmapCodec,
    lmapCodec,

    -- *** Enums
    shownBoundedEnumCodec,
    stringConstCodec,
    enumCodec,
    matchChoicesCodec,
    matchChoiceCodec,

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
