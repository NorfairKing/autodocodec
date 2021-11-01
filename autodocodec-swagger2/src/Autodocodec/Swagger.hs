module Autodocodec.Swagger where

import Autodocodec
import Data.Proxy
import Data.Swagger

declareNamedSchemaViaCodec :: HasCodec value => Proxy value -> Declare (Definitions Schema) NamedSchema
declareNamedSchemaViaCodec proxy = declareNamedSchemaVia codec proxy

declareNamedSchemaVia :: ValueCodec input output -> Proxy value -> Declare (Definitions Schema) NamedSchema
declareNamedSchemaVia codec Proxy = go codec
  where
    go :: Codec context input output -> Declare (Definitions Schema) NamedSchema
    go = undefined
