{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-duplicate-exports #-}

module Autodocodec.OpenAPI
  ( declareNamedSchemaViaCodec,
    declareNamedSchemaVia,

    -- * To makes sure we definitely export everything.
    module Autodocodec.OpenAPI.Schema,
    module Autodocodec.OpenAPI.DerivingVia,
  )
where

import Autodocodec.OpenAPI.DerivingVia ()
import Autodocodec.OpenAPI.Schema
