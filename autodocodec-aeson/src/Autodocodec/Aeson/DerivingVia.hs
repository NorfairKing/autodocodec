{-# OPTIONS_GHC -fno-warn-orphans #-}

module Autodocodec.Aeson.DerivingVia where

import Autodocodec
import Autodocodec.Aeson.Decode
import Autodocodec.Aeson.Encode
import Data.Aeson

instance HasCodec a => ToJSON (Autodocodec a) where
  toJSON = toJSONViaCodec . unAutodocodec

instance HasCodec a => FromJSON (Autodocodec a) where
  parseJSON = fmap Autodocodec <$> parseJSONViaCodec
