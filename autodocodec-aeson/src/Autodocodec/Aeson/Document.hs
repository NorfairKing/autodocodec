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
import GHC.Generics (Generic)

data JSONSchema
  = BoolSchema
  | StringSchema
  deriving (Show, Eq, Generic)

instance HasCodec JSONSchema where
  codec = undefined

instance ToJSON JSONSchema where
  toJSON = toJSONViaCodec

instance FromJSON JSONSchema where
  parseJSON = parseJSONViaCodec

jsonSchemaViaCodec :: forall a. HasCodec a => JSONSchema
jsonSchemaViaCodec = jsonSchemaVia (codec @a)

jsonSchemaVia :: Codec a a -> JSONSchema
jsonSchemaVia = go
  where
    go :: Codec a a -> JSONSchema
    go = \case
      BoolCodec -> BoolSchema
