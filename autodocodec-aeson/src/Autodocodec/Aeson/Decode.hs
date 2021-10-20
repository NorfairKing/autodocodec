{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Autodocodec.Aeson.Decode where

import Autodocodec
import Data.Aeson as JSON
import Data.Aeson.Types as JSON

parseJSONViaCodec :: HasCodec a => JSON.Value -> JSON.Parser a
parseJSONViaCodec = parseJSONVia codec

parseJSONVia :: Codec void a -> JSON.Value -> JSON.Parser a
parseJSONVia = flip go
  where
    go :: JSON.Value -> Codec void a -> JSON.Parser a
    go value = \case
      NullCodec -> pure ()
      BoolCodec -> parseJSON value
      StringCodec -> parseJSON value
      NumberCodec -> parseJSON value
      ObjectCodec c -> withObject "TODO" (\o -> goObject o c) value
      BimapCodec f _ c -> f <$> go value c
    -- PureCodec a -> pure a
    -- ApCodec cf ca -> go value cf <*> go value ca
    -- AltCodecs cs -> case cs of
    --   [] -> fail "Empty alternatives."
    --   (c : rest) -> go value c <|> go value (AltCodecs rest)

    goObject :: JSON.Object -> ObjectCodec void a -> JSON.Parser a
    goObject object_ = \case
      KeyCodec k c -> do
        value <- object_ JSON..: k
        go value c
      BimapObjectCodec f _ oc -> f <$> goObject object_ oc
      PureObjectCodec a -> pure a
      ApObjectCodec ocf oca -> goObject object_ ocf <*> goObject object_ oca
