{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Autodocodec.SwaggerSpec (spec) where

import Autodocodec
import Autodocodec.Aeson
import Autodocodec.Aeson.Schema
import Autodocodec.Swagger
import Autodocodec.Usage
import qualified Data.Aeson as JSON
import Data.Data
import Data.GenValidity
import Data.GenValidity.Aeson ()
import Data.GenValidity.Containers ()
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.Int
import Data.List.NonEmpty (NonEmpty (..))
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Word
import Test.QuickCheck
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  swaggerSchemaSpec @Bool "bool"
  swaggerSchemaSpec @Ordering "ordering"
  swaggerSchemaSpec @Char "char"
  swaggerSchemaSpec @Text "text"
  swaggerSchemaSpec @LT.Text "lazy-text"
  swaggerSchemaSpec @String "string"
  swaggerSchemaSpec @Scientific "scientific"
  swaggerSchemaSpec @JSON.Value "value"
  swaggerSchemaSpec @Int "int"
  swaggerSchemaSpec @Int8 "int8"
  swaggerSchemaSpec @Int16 "int16"
  swaggerSchemaSpec @Int32 "int32"
  swaggerSchemaSpec @Int64 "int64"
  swaggerSchemaSpec @Word "word"
  swaggerSchemaSpec @Word8 "word8"
  swaggerSchemaSpec @Word16 "word16"
  swaggerSchemaSpec @Word32 "word32"
  swaggerSchemaSpec @Word64 "word64"
  swaggerSchemaSpec @(Maybe Text) "maybe-text"
  swaggerSchemaSpec @(Either Bool Text) "either-bool-text"
  swaggerSchemaSpec @(Either (Either Bool Scientific) Text) "either-either-bool-scientific-text"
  swaggerSchemaSpec @[Text] "list-text"
  swaggerSchemaSpec @Example "example"
  swaggerSchemaSpec @Recursive "recursive"
  swaggerSchemaSpec @Via "via"

swaggerSchemaSpec :: forall a. (Show a, Eq a, Typeable a, GenValid a, HasCodec a) => FilePath -> Spec
swaggerSchemaSpec filePath =
  describe ("swaggerSchemaSpec @" <> nameOf @a) $ do
    it "outputs the same schema as before" $
      pureGoldenJSONFile
        ("test_resources/swagger/" <> filePath <> ".json")
        (JSON.toJSON (jsonSchemaViaCodec @a))
