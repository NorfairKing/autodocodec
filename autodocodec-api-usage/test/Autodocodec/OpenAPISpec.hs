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
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.GenValidity.Aeson ()
import Data.GenValidity.Containers ()
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Int
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Monoid as Monoid
import Data.OpenApi (Components (..), OpenApi (..))
import qualified Data.OpenApi as OpenAPI
import qualified Data.OpenApi.Declare as OpenAPI
import Data.Scientific
import Data.Semigroup (Dual)
import qualified Data.Semigroup as Semigroup
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Vector (Vector)
import Data.Void
import Data.Word
import Numeric.Natural
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity.Utils

openAPISchemaSpecAndViaDeclareSchemaRef :: forall a. (Typeable a, HasCodec a) => FilePath -> Spec
openAPISchemaSpecAndViaDeclareSchemaRef filePath = do
  openAPISchemaSpec @a filePath
  openAPISchemaSpecViaDeclareSchemaRef @a filePath

spec :: Spec
spec = do
  openAPISchemaSpec @NullUnit "null"
  openAPISchemaSpecViaDeclareSchemaRef @NullUnit "null"
  openAPISchemaSpec @Void "void"
  openAPISchemaSpecViaDeclareSchemaRef @Void "void"
  openAPISchemaSpec @(Either Void Bool) "either-void-bool"
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
  openAPISchemaSpec @Integer "integer"
  openAPISchemaSpec @Word "word"
  openAPISchemaSpec @Word8 "word8"
  openAPISchemaSpec @Word16 "word16"
  openAPISchemaSpec @Word32 "word32"
  openAPISchemaSpec @Word64 "word64"
  openAPISchemaSpec @Natural "natural"
  openAPISchemaSpec @(Maybe Text) "maybe-text"
  openAPISchemaSpec @(Either Bool Text) "either-bool-text"
  openAPISchemaSpec @(Either (Either Bool Scientific) Text) "either-either-bool-scientific-text"
  openAPISchemaSpec @(Vector Text) "vector-text"
  openAPISchemaSpec @[Text] "list-text"
  openAPISchemaSpec @(NonEmpty Text) "nonempty-text"
  openAPISchemaSpec @(Set Text) "set-text"
  openAPISchemaSpec @(Map Text Int) "map-text-int"
  openAPISchemaSpec @Day "day"
  openAPISchemaSpec @LocalTime "local-time"
  openAPISchemaSpec @UTCTime "utc-time"
  openAPISchemaSpec @TimeOfDay "time-of-day"
  xdescribe "doesn't hold" $ openAPISchemaSpec @ZonedTime "zoned-time"
  openAPISchemaSpec @DiffTime "difftime"
  openAPISchemaSpec @NominalDiffTime "nominal-difftime"
  openAPISchemaSpec @Fruit "fruit"
  openAPISchemaSpecViaDeclareSchemaRef @Fruit "fruit"
  openAPISchemaSpec @Example "example"
  openAPISchemaSpecViaDeclareSchemaRef @Example "example"
  openAPISchemaSpec @ListsExample "lists-example"
  openAPISchemaSpecViaDeclareSchemaRef @ListsExample "lists-example"
  openAPISchemaSpec @Recursive "recursive"
  openAPISchemaSpecViaDeclareSchemaRef @Recursive "recursive"
  openAPISchemaSpec @MutuallyRecursiveA "mutually-recursive"
  openAPISchemaSpecViaDeclareSchemaRef @MutuallyRecursiveA "mutually-recursive"
  openAPISchemaSpec @Via "via"
  openAPISchemaSpecViaDeclareSchemaRef @Via "via"
  openAPISchemaSpec @VeryComment "very-comment"
  openAPISchemaSpecViaDeclareSchemaRef @VeryComment "very-comment"
  openAPISchemaSpec @LegacyValue "legacy-value"
  openAPISchemaSpecViaDeclareSchemaRef @LegacyValue "legacy-value"
  openAPISchemaSpec @LegacyObject "legacy-object"
  openAPISchemaSpecViaDeclareSchemaRef @LegacyObject "legacy-object"
  openAPISchemaSpec @Ainur "ainur"
  openAPISchemaSpecViaDeclareSchemaRef @Ainur "ainur"
  openAPISchemaSpec @War "war"
  openAPISchemaSpecViaDeclareSchemaRef @War "war"
  openAPISchemaSpec @These "these"
  openAPISchemaSpecViaDeclareSchemaRef @These "these"
  openAPISchemaSpec @Expression "expression"
  openAPISchemaSpecViaDeclareSchemaRef @Expression "expression"
  openAPISchemaSpecAndViaDeclareSchemaRef @(Identity Text) "identity"
  openAPISchemaSpecAndViaDeclareSchemaRef @(Dual Text) "dual"
  openAPISchemaSpecAndViaDeclareSchemaRef @(Semigroup.First Text) "semigroup-first"
  openAPISchemaSpecAndViaDeclareSchemaRef @(Semigroup.Last Text) "semigroup-last"
  openAPISchemaSpecAndViaDeclareSchemaRef @(Monoid.First Text) "monoid-first"
  openAPISchemaSpecAndViaDeclareSchemaRef @(Monoid.Last Text) "monoid-last"
  openAPISchemaSpecAndViaDeclareSchemaRef @(Const Text Void) "const"

openAPISchemaSpec :: forall a. (Typeable a, HasCodec a) => FilePath -> Spec
openAPISchemaSpec filePath =
  describe ("openAPISchemaSpec @" <> nameOf @a) $ do
    let (definitions, _) = flip OpenAPI.runDeclare mempty $ do
          OpenAPI.NamedSchema mName schema <- declareNamedSchemaViaCodec (Proxy :: Proxy a)
          OpenAPI.declare [(fromMaybe (T.pack $ nameOf @a) mName, schema)]
          pure schema
    it "outputs the same schema as before" $
      let openAPI = mempty {_openApiComponents = mempty {_componentsSchemas = definitions}}
       in pureGoldenJSONFile
            ("test_resources/openapi-schema/" <> filePath <> ".json")
            (JSON.toJSON openAPI)

openAPISchemaSpecViaDeclareSchemaRef ::
  forall a.
  (Typeable a, HasCodec a) =>
  FilePath ->
  Spec
openAPISchemaSpecViaDeclareSchemaRef filePath =
  describe ("openAPISchemaSpecViaDeclareSchemaRef @" <> nameOf @a) $ do
    it "outputs the same schema as before" $
      let (definitions, reference) = OpenAPI.runDeclare (OpenAPI.declareSchemaRef (Proxy :: Proxy (Autodocodec a))) mempty
          json =
            JSON.object
              [ "definitions" JSON..= definitions,
                "reference" JSON..= reference
              ]
       in pureGoldenJSONFile
            ("test_resources/openapi-schema/declareSchemaRef/" <> filePath <> ".json")
            (JSON.toJSON json)
