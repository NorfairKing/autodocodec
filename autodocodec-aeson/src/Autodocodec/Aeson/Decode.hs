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
import qualified Data.Text as T
import Data.Vector (Vector)

-- | Parse a JSON Value via the codec for the type that is being parsed.
parseJSONViaCodec :: HasCodec a => JSON.Value -> JSON.Parser a
parseJSONViaCodec = parseJSONVia codec

-- | Parse a JSON Value via a codec for the type that is being parsed.
parseJSONVia :: ValueCodec void a -> JSON.Value -> JSON.Parser a
parseJSONVia = parseContextVia

-- | Parse via a general codec.
--
-- You probably won't need this. See 'parseJSONVia' and 'parseJSONViaCodec' instead.
parseContextVia :: Codec context void a -> context -> JSON.Parser a
parseContextVia = flip go
  where
    -- We use type-annotations here for readability of type information that is
    -- gathered to case-matching on GADTs, they aren't strictly necessary.
    go :: context -> Codec context void a -> JSON.Parser a
    go value = \case
      NullCodec -> case (value :: JSON.Value) of
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
      ArrayOfCodec mname c -> do
        vector <- case mname of
          Nothing -> parseJSON value
          Just name -> withArray (T.unpack name) pure value
        mapM (`go` c) (vector :: Vector JSON.Value)
      ObjectOfCodec mname c -> do
        object_ <- case mname of
          Nothing -> parseJSON value
          Just name -> withObject (T.unpack name) pure value
        (`go` c) (object_ :: JSON.Object)
      ObjectCodec -> parseJSON value :: JSON.Parser JSON.Object
      ValueCodec -> pure (value :: JSON.Value)
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
      RequiredKeyCodec k c _ -> do
        valueAtKey <- (value :: JSON.Object) JSON..: k
        go valueAtKey c
      OptionalKeyCodec k c _ -> do
        let mValueAtKey = HM.lookup k (value :: JSON.Object)
        forM mValueAtKey $ \valueAtKey -> go (valueAtKey :: JSON.Value) c
      OptionalKeyWithDefaultCodec k c defaultValue _ -> do
        let mValueAtKey = HM.lookup k (value :: JSON.Object)
        case mValueAtKey of
          Nothing -> pure defaultValue
          Just valueAtKey -> go (valueAtKey :: JSON.Value) c
      PureCodec a -> pure a
      ApCodec ocf oca -> go (value :: JSON.Object) ocf <*> go (value :: JSON.Object) oca
