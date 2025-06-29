{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.SwaggerSpec (spec) where

import Autodocodec
import Autodocodec.Swagger
import Autodocodec.Usage
import qualified Data.Aeson as JSON
import Data.DList (DList)
import Data.DList.DNonEmpty (DNonEmpty)
import Data.Data
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.GenValidity
import Data.GenValidity.Aeson ()
import Data.GenValidity.Containers ()
import Data.GenValidity.DList ()
import Data.GenValidity.DNonEmpty ()
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Int
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Monoid as Monoid
import Data.Scientific
import Data.Semigroup (Dual)
import qualified Data.Semigroup as Semigroup
import Data.Set (Set)
import Data.Swagger (Swagger (..))
import qualified Data.Swagger as Swagger
import qualified Data.Swagger.Declare as Swagger
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
import Test.Syd.Validity
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  swaggerSchemaSpec @NullUnit "null"
  swaggerSchemaSpec' @Void "void"
  swaggerSchemaSpec' @(Either Void Bool) "either-void-bool"
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
  swaggerSchemaSpec @Integer "integer"
  swaggerSchemaSpec @Word "word"
  swaggerSchemaSpec @Word8 "word8"
  swaggerSchemaSpec @Word16 "word16"
  swaggerSchemaSpec @Word32 "word32"
  swaggerSchemaSpec @Word64 "word64"
  swaggerSchemaSpec @Natural "natural"
  swaggerSchemaSpec @(Maybe Text) "maybe-text"
  swaggerSchemaSpec @(Either Bool Text) "either-bool-text"
  swaggerSchemaSpec @(Either (Either Bool Scientific) Text) "either-either-bool-scientific-text"
  swaggerSchemaSpec @(Vector Text) "vector-text"
  swaggerSchemaSpec @[Text] "list-text"
  swaggerSchemaSpec @(DList Text) "dlist-text"
  swaggerSchemaSpec @(NonEmpty Text) "nonempty-text"
  swaggerSchemaSpec @(DNonEmpty Text) "dnonempty-text"
  swaggerSchemaSpec @(Set Text) "set-text"
  swaggerSchemaSpec @(Map Text Int) "map-text-int"
  swaggerSchemaSpec @Day "day"
  swaggerSchemaSpec @LocalTime "local-time"
  swaggerSchemaSpec @UTCTime "utc-time"
  swaggerSchemaSpec @TimeOfDay "time-of-day"
  swaggerSchemaSpec' @ZonedTime "zoned-time"
  swaggerSchemaSpec @DiffTime "difftime"
  swaggerSchemaSpec @NominalDiffTime "nominal-difftime"
  swaggerSchemaSpec @Fruit "fruit"
  swaggerSchemaSpec @Shape "shape"
  swaggerSchemaSpec @Example "example"
  swaggerSchemaSpec @Derived "derived"
  swaggerSchemaSpec @Recursive "recursive"
  swaggerSchemaSpec @ListsExample "lists-example"
  swaggerSchemaSpec @MutuallyRecursiveA "mutually-recursive"
  swaggerSchemaSpec @Via "via"
  swaggerSchemaSpec @VeryComment "very-comment"
  swaggerSchemaSpec @LegacyValue "legacy-value"
  swaggerSchemaSpec @LegacyObject "legacy-object"
  swaggerSchemaSpec @Ainur "ainur"
  swaggerSchemaSpec @War "war"
  swaggerSchemaSpec @These "these"
  swaggerSchemaSpec @Expression "expression"
  swaggerSchemaSpec @(Identity Text) "identity"
  swaggerSchemaSpec @(Dual Text) "dual"
  swaggerSchemaSpec @(Semigroup.First Text) "semigroup-first"
  swaggerSchemaSpec @(Semigroup.Last Text) "semigroup-last"
  swaggerSchemaSpec @(Monoid.First Text) "monoid-first"
  swaggerSchemaSpec @(Monoid.Last Text) "monoid-last"
  swaggerSchemaSpec @(Const Text Void) "const"
  xdescribe "does not hold because of overlap" $ swaggerSchemaSpec @Overlap "overlap"

swaggerSchemaSpec :: forall a. (Show a, Typeable a, GenValid a, HasCodec a) => FilePath -> Spec
swaggerSchemaSpec filePath =
  describe ("swaggerSchemaSpec @" <> nameOf @a) $ do
    swaggerSchemaSpec' @a filePath

    let (definitions, s) = flip Swagger.runDeclare mempty $ do
          Swagger.NamedSchema mName schema <- declareNamedSchemaViaCodec (Proxy :: Proxy a)
          Swagger.declare [(fromMaybe (T.pack $ nameOf @a) mName, schema)]
          pure schema
    it "validates all encoded values" $
      forAllValid $ \(a :: a) ->
        let encoded = toJSONViaCodec a
         in case Swagger.validateJSON definitions s encoded of
              [] -> pure ()
              errors ->
                expectationFailure $
                  unlines
                    [ "Generated value did not pass the Swagger Schema validation, but it should have",
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
                      unwords
                        [ "codec",
                          showCodecABit (codec @a)
                        ],
                      show errors
                    ]

swaggerSchemaSpec' :: forall a. (Typeable a, HasCodec a) => FilePath -> Spec
swaggerSchemaSpec' filePath = do
  let (definitions, _) = flip Swagger.runDeclare mempty $ do
        Swagger.NamedSchema mName schema <- declareNamedSchemaViaCodec (Proxy :: Proxy a)
        Swagger.declare [(fromMaybe (T.pack $ nameOf @a) mName, schema)]
        pure schema

  it "outputs the same schema as before" $
    let swagger = mempty {_swaggerDefinitions = definitions}
     in pureGoldenJSONFile
          ("test_resources/swagger-schema/" <> filePath <> ".json")
          (JSON.toJSON swagger)
