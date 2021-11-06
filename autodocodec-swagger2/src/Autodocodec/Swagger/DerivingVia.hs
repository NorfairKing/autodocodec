{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Autodocodec.Swagger.DerivingVia where

import Autodocodec
import Autodocodec.Swagger.Schema
import Data.Proxy
import Data.Swagger as Swagger

-- | An instance for 'Autodocodec' that lets you use 'DerivingVia' to derive 'Swagger.ToSchema' if your type has a 'HasCodec' instance.
--
-- > deriving (Swagger.ToSchema) via (Autodocodec FooBar)
instance (HasCodec a) => Swagger.ToSchema (Autodocodec a) where
  declareNamedSchema (Proxy :: Proxy (Autodocodec a)) = declareNamedSchemaViaCodec (Proxy :: Proxy a)
