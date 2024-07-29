{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Autodocodec.OpenAPI.DerivingVia where

import Autodocodec (Autodocodec, HasCodec)
import Autodocodec.OpenAPI.Schema (declareNamedSchemaViaCodec)
import qualified Data.OpenApi as OpenAPI
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable)

-- | An instance for 'Autodocodec' that lets you use 'DerivingVia' to derive 'OpenAPI.ToSchema' if your type has a 'HasCodec' instance.
--
-- > deriving (OpenAPI.ToSchema) via (Autodocodec FooBar)
instance (Typeable a, HasCodec a) => OpenAPI.ToSchema (Autodocodec a) where
  -- This is declared like this as because now 'declareNamedSchema' takes no arguments, it can be memoized (like a top level variable is).
  -- As a result, the result of the 'let', that is the schema, should also be memoized,
  -- so with this definition schema in 'declaredNamedSchema' should only need to be calculated once IF the instance is of the form:
  -- > deriving (OpenAPI.ToSchema) via (Autodocodec FooBar)
  -- where FooBar is a concrete type.
  -- If the instance is of the form:
  -- > deriving via (Autodocodec (FooBar a)) instance (OpenAPI.ToSchema (FooBar a))
  -- this memoisation trick probably won't work (as 'declaredNamedSchema' actually is a function due to hiddent typeclass dictionary arguments)
  -- but it shouldn't hurt to try. See:
  -- https://stackoverflow.com/questions/77056264/caching-an-expensive-to-compute-result-in-a-class-instance
  declareNamedSchema = let schema = declareNamedSchemaViaCodec (Proxy :: Proxy a) in const schema
