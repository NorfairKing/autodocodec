{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Autodocodec.OpenAPI.DerivingVia where

import Autodocodec
import Autodocodec.OpenAPI.Schema
import Data.OpenApi as OpenAPI
import Data.Proxy
import Data.Typeable

-- | An instance for 'Autodocodec' that lets you use 'DerivingVia' to derive 'OpenAPI.ToSchema' if your type has a 'HasCodec' instance.
--
-- > deriving (OpenAPI.ToSchema) via (Autodocodec FooBar)
instance (Typeable a, HasCodec a) => OpenAPI.ToSchema (Autodocodec a) where
  declareNamedSchema (Proxy :: Proxy (Autodocodec a)) = declareNamedSchemaViaCodec (Proxy :: Proxy a)
