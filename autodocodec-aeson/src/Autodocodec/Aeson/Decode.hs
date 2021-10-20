{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Autodocodec.Aeson.Decode where

import Autodocodec
import Data.Aeson as JSON
import Data.Aeson.Types as JSON

parseJSONViaCodec :: HasCodec a => JSON.Value -> JSON.Parser a
parseJSONViaCodec = parseJSONVia codec

parseJSONVia :: Codec a a -> JSON.Value -> JSON.Parser a
parseJSONVia = flip go
  where
    go :: JSON.Value -> Codec a a -> JSON.Parser a
    go value = \case
      NullCodec -> pure ()
      BoolCodec -> parseJSON value
      StringCodec -> parseJSON value
