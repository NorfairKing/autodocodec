{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Aeson.Document where

import Autodocodec
import Autodocodec.Aeson.Decode
import Autodocodec.Aeson.Encode
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import Data.Text (Text)
import GHC.Generics (Generic)

data JSONSchema
  = AnySchema
  | NullSchema
  | BoolSchema
  | StringSchema
  | NumberSchema
  | -- | ValueSchema JSON.Value
    ObjectSchema !JSONObjectSchema
  deriving (Show, Eq, Generic)

data JSONObjectSchema
  = AnyObjectSchema
  | KeySchema !Text !JSONSchema
  | ApObjectSchema !JSONObjectSchema !JSONObjectSchema
  deriving (Show, Eq, Generic)

instance ToJSON JSONSchema where
  toJSON = \case
    AnySchema -> JSON.object []
    NullSchema -> JSON.object ["type" JSON..= ("null" :: Text)]
    BoolSchema -> JSON.object ["type" JSON..= ("boolean" :: Text)]
    StringSchema -> JSON.object ["type" JSON..= ("string" :: Text)]
    NumberSchema -> JSON.object ["type" JSON..= ("number" :: Text)]
    ObjectSchema _ -> JSON.object ["type" JSON..= ("object" :: Text)] -- TODO this is wrong

instance FromJSON JSONSchema where
  parseJSON = JSON.withObject "JSONSchema" $ \o -> do
    t <- o JSON..: "type"
    case t :: Text of
      "null" -> pure NullSchema
      "boolean" -> pure BoolSchema
      "string" -> pure StringSchema
      "number" -> pure NumberSchema
      "object" -> pure $ ObjectSchema AnyObjectSchema
      _ -> fail "unknown schema type."

jsonSchemaViaCodec :: forall a. HasCodec a => JSONSchema
jsonSchemaViaCodec = jsonSchemaVia (codec @a)

jsonSchemaVia :: Codec input output -> JSONSchema
jsonSchemaVia = go
  where
    go :: Codec input output -> JSONSchema
    go = \case
      NullCodec -> NullSchema
      BoolCodec -> BoolSchema
      StringCodec -> StringSchema
      NumberCodec -> NumberSchema
      -- EqCodec _ c -> go c -- TODO maybe we want to show the specific value?
      ObjectCodec oc -> ObjectSchema (goObject oc)
      BimapCodec _ _ c -> go c

    goObject :: ObjectCodec input output -> JSONObjectSchema
    goObject = \case
      KeyCodec k c -> KeySchema k (go c)
      BimapObjectCodec _ _ oc -> goObject oc
      PureObjectCodec _ -> AnyObjectSchema
      ApObjectCodec oc1 oc2 -> ApObjectSchema (goObject oc1) (goObject oc2)
