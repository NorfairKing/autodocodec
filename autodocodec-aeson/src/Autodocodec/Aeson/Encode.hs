{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Autodocodec.Aeson.Encode where

import Autodocodec
import Data.Aeson as JSON
import Data.Text (Text)

toJSONViaCodec :: HasCodec a => a -> JSON.Value
toJSONViaCodec = toJSONVia codec

toJSONVia :: Codec a a -> a -> JSON.Value
toJSONVia = flip go
  where
    go :: a -> Codec a a -> JSON.Value
    go a = \case
      NullCodec -> toJSON (a :: ())
      BoolCodec -> toJSON (a :: Bool)
      StringCodec -> toJSON (a :: Text)
