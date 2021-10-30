{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Autodocodec.Aeson.Decode where

import Autodocodec
import Control.Applicative
import Control.Monad
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T

parseJSONViaCodec :: HasCodec a => JSON.Value -> JSON.Parser a
parseJSONViaCodec = parseJSONVia codec

parseJSONVia :: ValueCodec void a -> JSON.Value -> JSON.Parser a
parseJSONVia = parseContextVia

parseContextVia :: Codec context void a -> context -> JSON.Parser a
parseContextVia = flip go
  where
    go :: context -> Codec context void a -> JSON.Parser a
    go value = \case
      ValueCodec -> pure value
      NullCodec -> case value of
        Null -> pure ()
        _ -> fail $ "Expected Null, but got: " <> show value
      BoolCodec mname -> case mname of
        Nothing -> parseJSON value
        Just name -> withBool (T.unpack name) pure value
      StringCodec mname -> case mname of
        Nothing -> parseJSON value
        Just name -> withText (T.unpack name) pure value
      NumberCodec mname -> case mname of
        Nothing -> parseJSON value
        Just name -> withScientific (T.unpack name) pure value
      ArrayCodec mname c -> do
        vector <- case mname of
          Nothing -> parseJSON value
          Just name -> withArray (T.unpack name) pure value
        mapM (`go` c) vector
      ObjectCodec mname c -> do
        object_ <- case mname of
          Nothing -> parseJSON value
          Just name -> withObject (T.unpack name) pure value
        (`go` c) object_
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
      RequiredKeyCodec k c -> do
        valueAtKey <- value JSON..: k
        go valueAtKey c
      OptionalKeyCodec k c -> do
        let mValueAtKey = HM.lookup k value
        forM mValueAtKey $ \valueAtKey -> go valueAtKey c
      DefaultCodec defaultValue _ c -> fromMaybe defaultValue <$> go value c
      PureCodec a -> pure a
      ApCodec ocf oca -> go value ocf <*> go value oca
