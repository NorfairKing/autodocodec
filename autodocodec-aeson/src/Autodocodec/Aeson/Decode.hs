{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Autodocodec.Aeson.Decode where

import Autodocodec
import Control.Applicative
import Control.Monad
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.Foldable

parseJSONViaCodec :: HasCodec a => JSON.Value -> JSON.Parser a
parseJSONViaCodec = parseJSONVia codec

parseJSONVia :: Codec void a -> JSON.Value -> JSON.Parser a
parseJSONVia = flip go
  where
    go :: JSON.Value -> Codec void a -> JSON.Parser a
    go value = \case
      NullCodec -> case value of
        Null -> pure ()
        _ -> fail $ "Expected Null, but got: " <> show value
      BoolCodec -> parseJSON value
      StringCodec -> parseJSON value
      NumberCodec -> parseJSON value
      ArrayCodec c -> withArray "TODO" (\a -> toList <$> mapM (`go` c) a) value
      ObjectCodec c -> withObject "TODO" (\o -> goObject o c) value
      BimapCodec f _ c -> f <$> go value c
      ExtraParserCodec f _ c -> do
        old <- go value c
        case f old of
          Left err -> fail err -- TODO better error message location?
          Right new -> pure new
      SelectCodec c1 c2 -> (Left <$> go value c1) <|> (Right <$> go value c2)

    goObject :: JSON.Object -> ObjectCodec void a -> JSON.Parser a
    goObject object_ = \case
      RequiredKeyCodec k c -> do
        value <- object_ JSON..: k
        go value c
      OptionalKeyCodec k c -> do
        mValue <- object_ JSON..:? k
        forM mValue $ \value -> go value c
      BimapObjectCodec f _ oc -> f <$> goObject object_ oc
      PureObjectCodec a -> pure a
      ApObjectCodec ocf oca -> goObject object_ ocf <*> goObject object_ oca
