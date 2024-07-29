{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Autodocodec.Swagger.DerivingVia where

import Autodocodec (Autodocodec, HasCodec)
import Autodocodec.Swagger.Schema (declareNamedSchemaViaCodec)
import Data.Proxy (Proxy (..))
import qualified Data.Swagger as Swagger

-- | An instance for 'Autodocodec' that lets you use 'DerivingVia' to derive 'Swagger.ToSchema' if your type has a 'HasCodec' instance.
--
-- > deriving (Swagger.ToSchema) via (Autodocodec FooBar)
instance (HasCodec a) => Swagger.ToSchema (Autodocodec a) where
  -- See comments in 'Autodocodec.OpenAPI.DerivingVia' for the reason why this is defined like this.
  declareNamedSchema = let schema = declareNamedSchemaViaCodec (Proxy :: Proxy a) in const schema
