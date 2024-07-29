{-# LANGUAGE ScopedTypeVariables #-}

module Autodocodec.Swagger.DerivingVia (AutodocodecSwagger (..)) where

import Autodocodec (HasCodec)
import Autodocodec.Swagger.Schema (declareNamedSchemaViaCodec)
import Data.Proxy (Proxy (..))
import qualified Data.Swagger as Swagger

newtype AutodocodecSwagger a = AutodocodecSwagger {unAutodocodecSwagger :: a}

-- | An non-orphan instance for 'AutodocodecSwagger' that lets you use 'DerivingVia' to derive 'Swagger.ToSchema' if your type has a 'HasCodec' instance.
--
-- > deriving (Swagger.ToSchema) via (AutodocodecSwagger FooBar)
instance (HasCodec a) => Swagger.ToSchema (AutodocodecSwagger a) where
  -- See comments in 'Autodocodec.OpenAPI.DerivingVia' for the reason why this is defined like this.
  declareNamedSchema = let schema = declareNamedSchemaViaCodec (Proxy :: Proxy a) in const schema
