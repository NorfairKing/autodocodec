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
  | ChoiceSchema ![JSONSchema]
  deriving (Show, Eq, Generic)

data JSONObjectSchema
  = AnyObjectSchema
  | KeySchema !Text !JSONSchema
  | ApObjectSchema !JSONObjectSchema !JSONObjectSchema
  deriving (Show, Eq, Generic)

instance HasCodec JSONSchema where
  codec = undefined

-- ChoiceCodec
--   ( [ bimapCodec
--         (const BoolSchema :: Text -> JSONSchema)
--         (const "boolean" :: JSONSchema -> Text)
--         (object (field "type" .== ("boolean" :: Text)))
--     ] ::
--       [Codec JSONSchema JSONSchema]
--   )
--   ( ( \case
--         BoolSchema ->
--           bimapCodec
--             (const BoolSchema :: Text -> JSONSchema)
--             (const "boolean" :: JSONSchema -> Text)
--             (object (field "type" .== "boolean"))
--     ) ::
--       JSONSchema -> Codec JSONSchema JSONSchema
--   )

instance ToJSON JSONSchema where
  toJSON = toJSONViaCodec

instance FromJSON JSONSchema where
  parseJSON = parseJSONViaCodec

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
      ChoiceCodec cs _ -> ChoiceSchema (map go cs)

    goObject :: ObjectCodec input output -> JSONObjectSchema
    goObject = \case
      KeyCodec k c -> KeySchema k (go c)
      BimapObjectCodec _ _ oc -> goObject oc
      PureObjectCodec _ -> AnyObjectSchema
      ApObjectCodec oc1 oc2 -> ApObjectSchema (goObject oc1) (goObject oc2)
