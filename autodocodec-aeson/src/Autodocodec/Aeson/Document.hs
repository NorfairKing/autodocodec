{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Aeson.Document where

import Autodocodec
import Autodocodec.Aeson.Decode
import Autodocodec.Aeson.Encode
import Data.Aeson as JSON
import Data.Text (Text)
import GHC.Generics (Generic)

data JSONSchema
  = AnySchema
  | NullSchema
  | BoolSchema
  | StringSchema
  | NumberSchema
  | ObjectSchema !JSONObjectSchema
  deriving (Show, Eq, Generic)

data JSONObjectSchema
  = AnyObjectSchema
  | KeySchema !Text !JSONSchema
  | ApObjectSchema !JSONObjectSchema !JSONObjectSchema
  deriving (Show, Eq, Generic)

instance HasCodec JSONSchema where
  codec = undefined

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
      ObjectCodec oc -> ObjectSchema (goObject oc)
      BimapCodec _ _ c -> go c

    goObject :: ObjectCodec input output -> JSONObjectSchema
    goObject = \case
      KeyCodec k c -> KeySchema k (go c)
      BimapObjectCodec _ _ oc -> goObject oc
      PureObjectCodec _ -> AnyObjectSchema
      ApObjectCodec oc1 oc2 -> ApObjectSchema (goObject oc1) (goObject oc2)
