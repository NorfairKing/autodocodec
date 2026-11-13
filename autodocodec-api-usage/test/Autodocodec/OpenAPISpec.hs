{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.OpenAPISpec (spec) where

import Autodocodec
import Autodocodec.OpenAPI
import Autodocodec.OpenAPI.DerivingVia
import Autodocodec.Usage
import qualified Data.Aeson as JSON
import Data.DList (DList)
import Data.DList.DNonEmpty (DNonEmpty)
import Data.Data
import Data.Foldable (toList)
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
  openAPISchemaSpec @StringWithBounds "string-with-bounds"
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
  openAPISchemaSpec @(DList Text) "dlist-text"
  openAPISchemaSpec @(NonEmpty Text) "nonempty-text"
  openAPISchemaSpec @(DNonEmpty Text) "dnonempty-text"
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
  openAPISchemaSpec @Shape "shape"
  openAPISchemaSpecViaDeclareSchemaRef @Shape "shape"
  openAPISchemaSpec @Example "example"
  openAPISchemaSpecViaDeclareSchemaRef @Example "example"
  openAPISchemaSpec @Derived "derived"
  openAPISchemaSpecViaDeclareSchemaRef @Derived "derived"
  openAPISchemaSpec @ListsExample "lists-example"
  openAPISchemaSpecViaDeclareSchemaRef @ListsExample "lists-example"
  openAPISchemaSpec @OptionalFields "optional-fields"
  openAPISchemaSpecViaDeclareSchemaRef @OptionalFields "optional-fields"
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
  openAPISchemaSpec @Overlap "overlap"
  -- Not covered because openapi3 has no 'OpenAPI.ToSchema' instance for them:
  -- Ordering, JSON.Value, Monoid.First, Monoid.Last, Const, DiffTime
  describe "ToSchema equivalence" $ do
    -- Primitives and numerics
    openAPIToSchemaEquivalenceSpec @Bool
    openAPIToSchemaEquivalenceSpec @Char
    openAPIToSchemaEquivalenceSpec @Text
    openAPIToSchemaEquivalenceSpec @LT.Text
    openAPIToSchemaEquivalenceSpec @String
    openAPIToSchemaEquivalenceSpec @Scientific
    -- Matching this needs changes to how 'HashMapCodec' is named and rendered.
    xdescribe "doesn't hold: codec is unnamed, has additionalProperties and no description" $
      openAPIToSchemaEquivalenceSpec @JSON.Object
    openAPIToSchemaEquivalenceSpec @Int
    openAPIToSchemaEquivalenceSpec @Int8
    openAPIToSchemaEquivalenceSpec @Int16
    openAPIToSchemaEquivalenceSpec @Int32
    openAPIToSchemaEquivalenceSpec @Int64
    openAPIToSchemaEquivalenceSpec @Integer
    openAPIToSchemaEquivalenceSpec @Word
    openAPIToSchemaEquivalenceSpec @Word8
    openAPIToSchemaEquivalenceSpec @Word16
    openAPIToSchemaEquivalenceSpec @Word32
    openAPIToSchemaEquivalenceSpec @Word64
    -- The default, 'False', so both schemas mean the same thing.
    xdescribe "doesn't hold: codec lacks exclusiveMinimum False" $
      openAPIToSchemaEquivalenceSpec @Natural
    -- Containers and wrappers
    -- The codec does accept null, so its schema is the accurate one.
    xdescribe "doesn't hold: codec sets nullable True" $
      openAPIToSchemaEquivalenceSpec @(Maybe Text)
    -- Naming it like openapi3 does would need Typeable constraints on the instance.
    xdescribe "doesn't hold: codec is unnamed" $
      openAPIToSchemaEquivalenceSpec @(Either Bool Text)
    openAPIToSchemaEquivalenceSpec @[Text]
    openAPIToSchemaEquivalenceSpec @(Vector Text)
    openAPIToSchemaEquivalenceSpec @(NonEmpty Text)
    -- The codec removes duplicates instead of rejecting them, so this would
    -- document a check that decoding doesn't do.
    xdescribe "doesn't hold: codec lacks uniqueItems True" $
      openAPIToSchemaEquivalenceSpec @(Set Text)
    openAPIToSchemaEquivalenceSpec @(Map Text Int)
    openAPIToSchemaEquivalenceSpec @(Identity Text)
    openAPIToSchemaEquivalenceSpec @(Dual Text)
    openAPIToSchemaEquivalenceSpec @(Semigroup.First Text)
    openAPIToSchemaEquivalenceSpec @(Semigroup.Last Text)
    -- Time
    openAPIToSchemaEquivalenceSpec @Day
    openAPIToSchemaEquivalenceSpec @LocalTime
    openAPIToSchemaEquivalenceSpec @UTCTime
    openAPIToSchemaEquivalenceSpec @TimeOfDay
    openAPIToSchemaEquivalenceSpec @ZonedTime
    openAPIToSchemaEquivalenceSpec @NominalDiffTime
    -- Records with openapi3's generic instances
    openAPIToSchemaEquivalenceSpec @GenericRecord
    openAPIToSchemaEquivalenceSpec @GenericOptionalRecord
    -- The field does accept null, so its schema is the accurate one, as for 'Maybe'.
    xdescribe "doesn't hold: codec sets nullable True on the or-null field" $
      openAPIToSchemaEquivalenceSpec @GenericOptionalOrNullRecord
  openAPIDocCompositionSpec

-- | How documentation composes when codecs are wrapped in more than one doc.
--
-- "Inner" is the doc closest to the value codec, "outer" the one applied around it.
openAPIDocCompositionSpec :: Spec
openAPIDocCompositionSpec =
  describe "doc composition" $ do
    it "puts the outer description before the inner one" $
      OpenAPI._schemaDescription (openAPISchemaOf ((textCodec <?> "inner") <?> "outer"))
        `shouldBe` Just "outer\ninner"
    it "uses the outer title" $
      OpenAPI._schemaTitle (openAPISchemaOf (withTitle "outer" (withTitle "inner" textCodec)))
        `shouldBe` Just "outer"
    it "uses the outer format" $
      OpenAPI._schemaFormat (openAPISchemaOf (withFormat "outer" (withFormat "inner" textCodec)))
        `shouldBe` Just "outer"
    it "uses the outer example" $
      OpenAPI._schemaExample (openAPISchemaOf (withExample "outer" (withExample "inner" textCodec)))
        `shouldBe` Just "outer"
    it "uses the outer multipleOf" $
      OpenAPI._schemaMultipleOf (openAPISchemaOf (withMultipleOf 2 (withMultipleOf 3 scientificCodec)))
        `shouldBe` Just 2
    it "uses a larger outer minItems" $
      OpenAPI._schemaMinItems (openAPISchemaOf (withMinItems 3 (nonEmptyCodec textCodec)))
        `shouldBe` Just 3
    it "keeps a larger inner minItems" $
      OpenAPI._schemaMinItems (openAPISchemaOf (withMinItems 0 (nonEmptyCodec textCodec)))
        `shouldBe` Just 1
    it "keeps docs of different kinds" $
      let s = openAPISchemaOf (withFormat "f" (withTitle "t" textCodec <?> "d"))
       in (OpenAPI._schemaFormat s, OpenAPI._schemaTitle s, OpenAPI._schemaDescription s)
            `shouldBe` (Just "f", Just "t", Just "d")
    it "puts a field's doc before the doc of its value" $
      let c = object "Field" (requiredFieldWith "key" (textCodec <?> "value doc") "field doc")
       in case toList (OpenAPI._schemaProperties (openAPISchemaOf c)) of
            [OpenAPI.Inline p] -> OpenAPI._schemaDescription p `shouldBe` Just "field doc\nvalue doc"
            ps -> expectationFailure $ "Expected one inline property, got: " <> show ps

    describe "field docs on named types" $ do
      -- A named type's property is a '$ref', and OpenAPI 3.0 ignores anything
      -- next to a '$ref', so a field's own doc goes on an 'allOf' around it.
      let dayRef = OpenAPI.Ref (OpenAPI.Reference "Day")
      it "puts a field's doc on an allOf around the reference" $
        openAPIPropertiesOf (object "Field" (requiredFieldWith "key" (codec @Day) "field doc"))
          `shouldBe` [OpenAPI.Inline mempty {OpenAPI._schemaDescription = Just "field doc", OpenAPI._schemaAllOf = Just [dayRef]}]
      it "leaves the named type's definition alone" $
        let (defs, _) = openAPIDeclarationOf (object "Field" (requiredFieldWith "key" (codec @Day) "field doc"))
         in map (\s -> (OpenAPI._schemaDescription s, OpenAPI._schemaFormat s)) (toList defs)
              `shouldBe` [(Nothing, Just "date")]
      it "keeps a field's doc for a time type" $
        openAPIPropertiesOf (object "Field" (requiredFieldWith "key" (codec @UTCTime) "when"))
          `shouldBe` [OpenAPI.Inline mempty {OpenAPI._schemaDescription = Just "when", OpenAPI._schemaAllOf = Just [OpenAPI.Ref (OpenAPI.Reference "UTCTime")]}]
      it "puts a field's default on an allOf around the reference" $
        openAPIPropertiesOf (object "Field" (optionalFieldWithDefaultWith' "key" (named "Name" textCodec) "dflt"))
          `shouldBe` [OpenAPI.Inline mempty {OpenAPI._schemaDefault = Just "dflt", OpenAPI._schemaAllOf = Just [OpenAPI.Ref (OpenAPI.Reference "Name")]}]
      it "leaves an undocumented field as a plain reference" $
        openAPIPropertiesOf (object "Field" (requiredFieldWith' "key" (codec @Day)))
          `shouldBe` [dayRef]
      it "leaves a field with an empty doc as a plain reference" $
        openAPIPropertiesOf (object "Field" (requiredFieldWith "key" (codec @Day) ""))
          `shouldBe` [dayRef]

-- | The properties of the object schema that a codec documents.
openAPIPropertiesOf :: forall a. JSONCodec a -> [OpenAPI.Referenced OpenAPI.Schema]
openAPIPropertiesOf = toList . OpenAPI._schemaProperties . openAPISchemaOf

-- | The definitions and schema that a codec documents.
openAPIDeclarationOf :: forall a. JSONCodec a -> (OpenAPI.Definitions OpenAPI.Schema, OpenAPI.Schema)
openAPIDeclarationOf c =
  OpenAPI._namedSchemaSchema
    <$> OpenAPI.runDeclare (declareNamedSchemaVia c (Proxy :: Proxy a)) mempty

-- | The schema that a codec documents, without its name or definitions.
openAPISchemaOf :: forall a. JSONCodec a -> OpenAPI.Schema
openAPISchemaOf c =
  OpenAPI._namedSchemaSchema . snd $
    OpenAPI.runDeclare (declareNamedSchemaVia c (Proxy :: Proxy a)) mempty

-- | Check that the 'OpenAPI.ToSchema' instance that is defined for @a@ outside
-- of autodocodec produces exactly the same output as the codec-based one.
openAPIToSchemaEquivalenceSpec ::
  forall a.
  (HasCodec a, OpenAPI.ToSchema a) =>
  Spec
openAPIToSchemaEquivalenceSpec =
  describe ("openAPIToSchemaEquivalenceSpec @" <> nameOf @a) $ do
    it "has the same declareNamedSchema" $
      let native = OpenAPI.runDeclare (OpenAPI.declareNamedSchema (Proxy :: Proxy a)) mempty
          viaCodec = OpenAPI.runDeclare (declareNamedSchemaViaCodec (Proxy :: Proxy a)) mempty
       in viaCodec `shouldBe` native
    it "has the same declareSchemaRef" $
      let native = OpenAPI.runDeclare (OpenAPI.declareSchemaRef (Proxy :: Proxy a)) mempty
          viaCodec = OpenAPI.runDeclare (OpenAPI.declareSchemaRef (Proxy :: Proxy (AutodocodecOpenApi a))) mempty
       in viaCodec `shouldBe` native

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
      let (definitions, reference) = OpenAPI.runDeclare (OpenAPI.declareSchemaRef (Proxy :: Proxy (AutodocodecOpenApi a))) mempty
          json =
            JSON.object
              [ "definitions" JSON..= definitions,
                "reference" JSON..= reference
              ]
       in pureGoldenJSONFile
            ("test_resources/openapi-schema/declareSchemaRef/" <> filePath <> ".json")
            (JSON.toJSON json)
