{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-duplicate-exports #-}

module Autodocodec.Yaml
  ( -- * Encoding and decoding Yaml
    encodeYamlViaCodec,
    eitherDecodeYamlViaCodec,

    -- * Reading Yaml Files
    readYamlConfigFile,
    readFirstYamlConfigFile,

    -- * Producing a Yaml schema
    renderColouredSchemaViaCodec,
    renderColouredSchemaVia,
    renderPlainSchemaViaCodec,
    renderPlainSchemaVia,
    schemaChunksViaCodec,
    schemaChunksVia,
    jsonSchemaChunks,

    -- * Instantiating @ToYaml@
    toYamlViaCodec,
    toYamlVia,

    -- * To makes sure we definitely export everything.
    module Autodocodec.Yaml.Schema,
    module Autodocodec.Yaml.IO,
    module Autodocodec.Yaml.Encode,
  )
where

import Autodocodec
import Autodocodec.Yaml.Encode
import Autodocodec.Yaml.IO
import Autodocodec.Yaml.Schema
import Data.ByteString
import qualified Data.Yaml as Yaml

-- | Encode a value as a Yaml 'ByteString' via its type's 'codec'.
encodeYamlViaCodec :: HasCodec a => a -> ByteString
encodeYamlViaCodec = Yaml.encode . Autodocodec

-- | Parse a Yaml 'ByteString' using a type's 'codec'.
eitherDecodeYamlViaCodec :: HasCodec a => ByteString -> Either Yaml.ParseException a
eitherDecodeYamlViaCodec = fmap unAutodocodec . Yaml.decodeEither'
