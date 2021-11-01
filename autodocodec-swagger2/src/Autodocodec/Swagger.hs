{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Autodocodec.Swagger where

import Autodocodec
import qualified Data.Aeson as JSON
import Data.Proxy
import Data.Scientific
import Data.Swagger
import Data.Swagger.Declare
import Data.Text (Text)

declareNamedSchemaViaCodec :: HasCodec value => Proxy value -> Declare (Definitions Schema) NamedSchema
declareNamedSchemaViaCodec proxy = declareNamedSchemaVia codec proxy

declareNamedSchemaVia :: JSONCodec value -> Proxy value -> Declare (Definitions Schema) NamedSchema
declareNamedSchemaVia c Proxy = go c
  where
    go :: Codec context input output -> Declare (Definitions Schema) NamedSchema
    go = \case
      BoolCodec mname -> NamedSchema mname <$> declareSchema (Proxy :: Proxy Bool)
      StringCodec mname -> NamedSchema mname <$> declareSchema (Proxy :: Proxy Text)
      NumberCodec mname -> NamedSchema mname <$> declareSchema (Proxy :: Proxy Scientific)
      ObjectCodec -> declareNamedSchema (Proxy :: Proxy JSON.Object)
      _ -> pure $ NamedSchema Nothing mempty -- TODO
