{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-duplicate-exports #-}

module Autodocodec.Swagger
  ( declareNamedSchemaViaCodec,
    declareNamedSchemaVia,

    -- * To makes sure we definitely export everything.
    module Autodocodec.Swagger.Schema,
    module Autodocodec.Swagger.DerivingVia,
  )
where

import Autodocodec.Swagger.DerivingVia ()
import Autodocodec.Swagger.Schema
