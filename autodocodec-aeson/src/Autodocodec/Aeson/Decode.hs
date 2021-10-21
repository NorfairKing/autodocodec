{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Autodocodec.Aeson.Decode where

import Autodocodec
import Control.Applicative
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
    -- ChoiceCodec cs _ -> goChoice value cs

    goChoice :: JSON.Value -> [Codec void a] -> JSON.Parser a
    goChoice value = \case
      [] -> fail "No choices left." -- TODO better error
      (c : cs) -> go value c <|> goChoice value cs

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
      -- EqObjectCodec o oc -> do
      --   o' <- goObject object_ oc
      --   guard (o == o')
      --   pure o'
      BimapObjectCodec f _ oc -> f <$> goObject object_ oc
      PureObjectCodec a -> pure a
      ApObjectCodec ocf oca -> goObject object_ ocf <*> goObject object_ oca
