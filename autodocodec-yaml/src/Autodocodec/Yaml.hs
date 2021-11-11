{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-duplicate-exports #-}

module Autodocodec.Yaml
  ( -- ** Yaml schemas
    renderColouredSchemaViaCodec,
    renderColouredSchemaVia,
    renderPlainSchemaViaCodec,
    renderPlainSchemaVia,
    schemaChunksViaCodec,
    schemaChunksVia,
    jsonSchemaChunks,

    -- ** ToYaml implementation
    toYamlViaCodec,
    toYamlVia,

    -- * To makes sure we definitely export everything.
    module Autodocodec.Yaml.Encode,
  )
where

import Autodocodec.Yaml.Document
import Autodocodec.Yaml.Encode
