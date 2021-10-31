{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Autodocodec
  ( -- * Codec
    JSONCodec,
    HasCodec (..),

    -- * Writing a codec
    object,

    -- ** Field codecs
    requiredField,
    optionalField,
    optionalFieldOrNull,
    optionalFieldWithDefault,
    requiredFieldWith,
    optionalFieldWith,
    optionalFieldOrNullWith,
    optionalFieldWithDefaultWith,

    -- ** Documentation-less versions of field codecs
    requiredField',
    optionalField',
    optionalFieldOrNull',
    optionalFieldWithDefault',
    requiredFieldWith',
    optionalFieldWith',
    optionalFieldOrNullWith',
    optionalFieldWithDefaultWith',

    -- ** Writing your own value codecs.
    (<?>),
    (<??>),
    (.=),
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

    -- *** Mapping
    rmapCodec,
    lmapCodec,
    dimapCodec,
    bimapCodec,

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

    -- ** Internals you most likely don't need
    showCodecABit,

    -- * To make sure we definitely export everything
    module Autodocodec.Class,
    module Autodocodec.Codec,
  )
where

import Autodocodec.Class
import Autodocodec.Codec
