{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-duplicate-exports #-}

module Autodocodec.Aeson
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

    -- * To makes sure we definitely export everything.
    module Autodocodec.Aeson.Decode,
    module Autodocodec.Aeson.Encode,
  )
where

import Autodocodec.Aeson.Decode
import Autodocodec.Aeson.Encode
import Autodocodec.Class
import Autodocodec.DerivingVia
import qualified Data.Aeson as Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as LB

-- | Encode a value as a JSON 'LB.ByteString' via its type's 'codec'.
encodeViaCodec :: HasCodec a => a -> LB.ByteString
encodeViaCodec = Aeson.encode . Autodocodec

-- | Parse a JSON 'LB.ByteString' using a type's 'codec'.
eitherDecodeViaCodec :: HasCodec a => LB.ByteString -> Either String a
eitherDecodeViaCodec = fmap unAutodocodec . Aeson.eitherDecode
