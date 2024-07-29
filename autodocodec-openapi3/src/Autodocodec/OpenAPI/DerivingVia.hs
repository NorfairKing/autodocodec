{-# LANGUAGE ScopedTypeVariables #-}

module Autodocodec.OpenAPI.DerivingVia where

import Autodocodec.Class (HasCodec)
import Autodocodec.OpenAPI.Schema (declareNamedSchemaViaCodec)
import qualified Data.OpenApi as OpenAPI
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable)

newtype AutodocodecOpenApi a = AutodocodecOpenApi {unAutodocodecSwagger :: a}

-- | An instance for 'Autodocodec' that lets you use 'DerivingVia' to derive 'OpenAPI.ToSchema' if your type has a 'HasCodec' instance.
--
-- > deriving (OpenAPI.ToSchema) via (Autodocodec FooBar)
instance (Typeable a, HasCodec a) => OpenAPI.ToSchema (AutodocodecOpenApi a) where
  -- See comments in 'Autodocodec.OpenAPI.DerivingVia' for the reason why this is defined like this.
  declareNamedSchema = let schema = declareNamedSchemaViaCodec (Proxy :: Proxy a) in const schema
