{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Autodocodec.OpenAPISpec (spec) where

import Autodocodec
import Autodocodec.OpenAPI
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
import Data.OpenApi (Components (..), OpenApi (..))
import qualified Data.OpenApi as OpenAPI
import qualified Data.OpenApi.Declare as OpenAPI
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Word
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  openAPISchemaSpec @NullUnit "null"
  openAPISchemaSpec @Bool "bool"
  openAPISchemaSpec @Ordering "ordering"
  openAPISchemaSpec @Char "char"
  openAPISchemaSpec @Text "text"
  openAPISchemaSpec @LT.Text "lazy-text"
  openAPISchemaSpec @String "string"
  openAPISchemaSpec @Scientific "scientific"
  openAPISchemaSpec @JSON.Object "object"
  openAPISchemaSpec @JSON.Value "value"
  openAPISchemaSpec @Int "int"
  openAPISchemaSpec @Int8 "int8"
  openAPISchemaSpec @Int16 "int16"
  openAPISchemaSpec @Int32 "int32"
  openAPISchemaSpec @Int64 "int64"
  openAPISchemaSpec @Word "word"
  openAPISchemaSpec @Word8 "word8"
  openAPISchemaSpec @Word16 "word16"
  openAPISchemaSpec @Word32 "word32"
  openAPISchemaSpec @Word64 "word64"
  openAPISchemaSpec @(Maybe Text) "maybe-text"
  openAPISchemaSpec @(Either Bool Text) "either-bool-text"
  openAPISchemaSpec @(Either (Either Bool Scientific) Text) "either-either-bool-scientific-text"
  openAPISchemaSpec @[Text] "list-text"
  openAPISchemaSpec @(NonEmpty Text) "nonempty-text"
  openAPISchemaSpec @Fruit "fruit"
  openAPISchemaSpec @Example "example"
  openAPISchemaSpec @Recursive "recursive"
  openAPISchemaSpec @Via "via"
  openAPISchemaSpec @VeryComment "very-comment"

openAPISchemaSpec :: forall a. (Show a, Eq a, Typeable a, GenValid a, HasCodec a) => FilePath -> Spec
openAPISchemaSpec filePath =
  describe ("openAPISchemaSpec @" <> nameOf @a) $ do
    let (definitions, s) = flip OpenAPI.runDeclare mempty $ do
          OpenAPI.NamedSchema mName schema <- declareNamedSchemaViaCodec (Proxy :: Proxy a)
          OpenAPI.declare [(fromMaybe (T.pack $ nameOf @a) mName, schema)]
          pure schema
    it "outputs the same schema as before" $
      let openAPI = mempty {_openApiComponents = mempty {_componentsSchemas = definitions}}
       in pureGoldenJSONFile
            ("test_resources/openapi-schema/" <> filePath <> ".json")
            (JSON.toJSON openAPI)

    -- Does not handle 'anyOf' correctly, I think.
    it "validates all encoded values" $
      forAllValid $ \(a :: a) ->
        let encoded = toJSONViaCodec a
         in case OpenAPI.validateJSON definitions s encoded of
              [] -> pure ()
              errors ->
                expectationFailure $
                  unlines
                    [ "Generated value did not pass the OpenAPI Schema validation, but it should have",
                      unwords
                        [ "value",
                          ppShow a
                        ],
                      unwords
                        [ "encoded",
                          ppShow encoded
                        ],
                      unwords
                        [ "schema",
                          ppShow s
                        ],
                      show errors
                    ]
