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
      NumberCodec -> parseJSON value
      ObjectCodec c -> withObject "TODO" (\o -> goObject o c) value
      PureCodec a -> pure a

    goObject :: JSON.Object -> ObjectCodec a a -> JSON.Parser a
    goObject object = \case
      PureObjectCodec a -> pure a
