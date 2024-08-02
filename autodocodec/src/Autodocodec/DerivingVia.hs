module Autodocodec.DerivingVia where

import Autodocodec.Aeson.Decode (parseJSONViaCodec)
import Autodocodec.Aeson.Encode (toEncodingViaCodec, toJSONViaCodec)
import Autodocodec.Class (HasCodec)
import qualified Data.Aeson as JSON

-- | 'Autodocodec' is a wrapper to provide codec-based deriving strategies.
--
-- === Example usage
--
-- > data Via = Via {viaOne :: !Text, viaTwo :: !Text}
-- >   deriving stock (Show, Eq, Generic)
-- >   deriving (FromJSON, ToJSON) via (Autodocodec Via)
-- >
-- > instance HasCodec Via where
-- >   codec =
-- >     object "Via" $
-- >       Via
-- >         <$> requiredField "one" "first field" .= viaOne
-- >         <*> requiredField "two" "second field" .= viaTwo
newtype Autodocodec a = Autodocodec {unAutodocodec :: a}

instance (HasCodec a) => JSON.ToJSON (Autodocodec a) where
  toJSON = toJSONViaCodec . unAutodocodec
  toEncoding = toEncodingViaCodec . unAutodocodec

instance (HasCodec a) => JSON.FromJSON (Autodocodec a) where
  parseJSON = fmap Autodocodec <$> parseJSONViaCodec
