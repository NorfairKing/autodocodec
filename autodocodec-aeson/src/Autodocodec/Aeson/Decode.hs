{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Autodocodec.Aeson.Decode where

import Autodocodec
import Control.Applicative
import Control.Monad
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

parseJSONViaCodec :: HasCodec a => JSON.Value -> JSON.Parser a
parseJSONViaCodec = parseJSONVia codec

parseJSONVia :: Codec void a -> JSON.Value -> JSON.Parser a
parseJSONVia = flip go
  where
    go :: JSON.Value -> Codec void a -> JSON.Parser a
    go value = \case
      ValueCodec -> pure value
      NullCodec -> case value of
        Null -> pure ()
        _ -> fail $ "Expected Null, but got: " <> show value
      BoolCodec -> parseJSON value
      StringCodec -> parseJSON value
      NumberCodec -> parseJSON value
      ArrayCodec mname c -> withArray (maybe "Unnamed" T.unpack mname) (\a -> toList <$> mapM (`go` c) a) value
      ObjectCodec mname c -> withObject (maybe "Unnamed" T.unpack mname) (\o -> goObject o c) value
      EqCodec expected c -> do
        actual <- go value c
        if expected == actual
          then pure actual
          else fail $ unwords ["Expected", show expected, "but got", show actual]
      MapCodec f _ c -> do
        old <- go value c
        case f old of
          Left err -> fail err -- TODO better error message location?
          Right new -> pure new
      EitherCodec c1 c2 -> (Left <$> go value c1) <|> (Right <$> go value c2)
      CommentCodec _ c -> go value c
      ReferenceCodec _ c -> go value c

    goObject :: JSON.Object -> ObjectCodec void a -> JSON.Parser a
    goObject object_ = \case
      RequiredKeyCodec k c -> do
        value <- object_ JSON..: k
        go value c
      OptionalKeyCodec k c -> do
        let mValue = HM.lookup k object_
        forM mValue $ \value -> go value c
      BimapObjectCodec f _ oc -> f <$> goObject object_ oc
      PureObjectCodec a -> pure a
      ApObjectCodec ocf oca -> goObject object_ ocf <*> goObject object_ oca
