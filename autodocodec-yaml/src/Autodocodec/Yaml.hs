{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-duplicate-exports #-}

module Autodocodec.Yaml
  ( -- * Reading Yaml Files
    readYamlConfigFile,
    readFirstYamlConfigFile,

    -- * Yaml schemas
    renderColouredSchemaViaCodec,
    renderColouredSchemaVia,
    renderPlainSchemaViaCodec,
    renderPlainSchemaVia,
    schemaChunksViaCodec,
    schemaChunksVia,
    jsonSchemaChunks,

    -- * ToYaml implementation
    toYamlViaCodec,
    toYamlVia,

    -- * To makes sure we definitely export everything.
    module Autodocodec.Yaml.Document,
    module Autodocodec.Yaml.IO,
    module Autodocodec.Yaml.Encode,
  )
where

import Autodocodec.Yaml.Document
import Autodocodec.Yaml.Encode
import Autodocodec.Yaml.IO
