module Autodocodec
  ( -- * Codec
    JSONCodec,
    ValueCodec,
    ObjectCodec,
    HasCodec (..),

    -- * Writing a codec
    object,
    dimapCodec,
    (.=),
    eitherCodec,
    maybeCodec,
    (<?>),
    (<??>),
    nullCodec,
    boolCodec,
    textCodec,
    stringCodec,
    scientificCodec,
    boundedIntegerCodec,
    literalText,
    literalTextValue,
    shownBoundedEnumCodec,
    stringConstCodec,
    enumCodec,
    matchChoicesCodec,
    matchChoiceCodec,

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

    -- * Bare codec
    Codec (..),
    rmapCodec,
    lmapCodec,
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
