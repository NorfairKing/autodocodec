{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Autodocodec.SwaggerSpec (spec) where

import Autodocodec
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
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Scientific
import Data.Swagger (Swagger (..))
import qualified Data.Swagger as Swagger
import qualified Data.Swagger.Declare as Swagger
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Word
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  swaggerSchemaSpec @NullUnit "null"
  swaggerSchemaSpec @Bool "bool"
  swaggerSchemaSpec @Ordering "ordering"
  swaggerSchemaSpec @Char "char"
  swaggerSchemaSpec @Text "text"
  swaggerSchemaSpec @LT.Text "lazy-text"
  swaggerSchemaSpec @String "string"
  swaggerSchemaSpec @Scientific "scientific"
  swaggerSchemaSpec @JSON.Object "object"
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
  swaggerSchemaSpec @(NonEmpty Text) "nonempty-text"
  swaggerSchemaSpec @Fruit "fruit"
  swaggerSchemaSpec @Example "example"
  swaggerSchemaSpec @Recursive "recursive"
  swaggerSchemaSpec @Via "via"

swaggerSchemaSpec :: forall a. (Show a, Eq a, Typeable a, GenValid a, HasCodec a) => FilePath -> Spec
swaggerSchemaSpec filePath =
  describe ("swaggerSchemaSpec @" <> nameOf @a) $ do
    it "outputs the same schema as before" $
      let definitions = flip Swagger.execDeclare mempty $ do
            Swagger.NamedSchema mName schema <- declareNamedSchemaViaCodec (Proxy :: Proxy a)
            Swagger.declare [(fromMaybe (T.pack $ nameOf @a) mName, schema)]
            pure ()
          swagger = mempty {_swaggerDefinitions = definitions}
       in pureGoldenJSONFile
            ("test_resources/swagger-schema/" <> filePath <> ".json")
            (JSON.toJSON swagger)
