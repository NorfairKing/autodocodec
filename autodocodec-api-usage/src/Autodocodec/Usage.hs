{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | Autodocodec API usage tests
--
-- This module contains so-called "API Usage" Tests.
-- This means that this module tests that its usage of the API is still supported.
-- Consequentially, we must be careful about refactoring any code in this module.
module Autodocodec.Usage where

import Autodocodec
import Autodocodec.Aeson ()
import Autodocodec.Multipart
import Autodocodec.OpenAPI ()
import Autodocodec.OpenAPI.DerivingVia (AutodocodecOpenApi)
import Autodocodec.Swagger ()
import Autodocodec.Swagger.DerivingVia (AutodocodecSwagger)
import Control.Applicative
import Control.DeepSeq
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import Data.Functor.Identity (Identity)
import Data.GenValidity
import Data.GenValidity.Aeson ()
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import qualified Data.Monoid as Monoid
import qualified Data.OpenApi as OpenAPI
import qualified Data.Semigroup as Semigroup
import qualified Data.Swagger as Swagger
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import GHC.Generics (Generic)
import Servant.Multipart
import Servant.Multipart.API as Servant
import Test.QuickCheck

-- | A type that's encoded as @null@.
data NullUnit = NullUnit
  deriving (Show, Eq, Generic)
  deriving (OpenAPI.ToSchema) via AutodocodecOpenApi NullUnit

instance Validity NullUnit

instance NFData NullUnit

instance GenValid NullUnit where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec NullUnit where
  codec = dimapCodec (\() -> NullUnit) (\NullUnit -> ()) nullCodec

instance FromJSON NullUnit where
  parseJSON = \case
    JSON.Null -> pure NullUnit
    _ -> fail "not a null."

instance ToJSON NullUnit where
  toJSON NullUnit = JSON.Null

-- | A simple enum type
data Fruit
  = Apple
  | Orange
  | Banana
  | Melon
  deriving (Show, Eq, Generic, Enum, Bounded)
  deriving (OpenAPI.ToSchema) via AutodocodecOpenApi Fruit

instance Validity Fruit

instance NFData Fruit

instance GenValid Fruit where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec Fruit where
  codec = shownBoundedEnumCodec

instance FromJSON Fruit

instance ToJSON Fruit

data Shape
  = ShapeCircle
  | ShapeSquare
  | ShapeRectangle
  deriving (Show, Eq, Generic, Enum, Bounded)
  deriving (FromJSON, ToJSON) via Autodocodec Shape
  deriving (OpenAPI.ToSchema) via AutodocodecOpenApi Shape

instance Validity Shape

instance NFData Shape

instance GenValid Shape where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec Shape where
  codec = boundedEnumCodec $ \case
    ShapeCircle -> "circle"
    ShapeSquare -> "square"
    ShapeRectangle -> "rectangle"

-- | A complex example type
data Example = Example
  { exampleText :: !Text,
    exampleBool :: !Bool,
    exampleRequiredMaybe :: !(Maybe Text),
    exampleOptional :: !(Maybe Text),
    exampleOptionalOrNull :: !(Maybe Text),
    exampleOptionalWithDefault :: !Text,
    exampleOptionalWithNullDefault :: ![Text],
    exampleSingleOrList :: ![Text],
    exampleFruit :: !Fruit,
    exampleShape :: !Shape
  }
  deriving (Show, Eq, Generic)
  deriving (OpenAPI.ToSchema) via (AutodocodecOpenApi Example)

instance Validity Example

instance NFData Example

instance GenValid Example where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec Example where
  codec = object "Example" objectCodec

instance HasObjectCodec Example where
  objectCodec =
    Example
      <$> requiredField "text" "a text" .= exampleText
      <*> requiredField "bool" "a bool" .= exampleBool
      <*> requiredField "maybe" "a maybe text" .= exampleRequiredMaybe
      <*> optionalField "optional" "an optional text" .= exampleOptional
      <*> optionalFieldOrNull "optional-or-null" "an optional-or-null text" .= exampleOptionalOrNull
      <*> optionalFieldWithDefault "optional-with-default" "foobar" "an optional text with a default" .= exampleOptionalWithDefault
      <*> optionalFieldWithOmittedDefault "optional-with-null-default" [] "an optional list of texts with a default empty list where the empty list would be omitted" .= exampleOptionalWithNullDefault
      <*> optionalFieldWithOmittedDefaultWith "single-or-list" (singleOrListCodec codec) [] "an optional list that can also be specified as a single element" .= exampleSingleOrList
      <*> requiredField "fruit" "fruit!!" .= exampleFruit
      <*> requiredField "shape" "shape!?" .= exampleShape

instance ToJSON Example where
  toJSON Example {..} =
    JSON.object $
      concat
        [ [ "text" JSON..= exampleText,
            "bool" JSON..= exampleBool,
            "maybe" JSON..= exampleRequiredMaybe,
            "fruit" JSON..= exampleFruit,
            "shape" JSON..= exampleShape,
            "optional-with-default" JSON..= exampleOptionalWithDefault
          ],
          [ "optional" JSON..= opt
            | opt <- maybeToList exampleOptional
          ],
          [ "optional-or-null" JSON..= opt
            | opt <- maybeToList exampleOptionalOrNull
          ],
          [ "optional-with-null-default" JSON..= exampleOptionalWithNullDefault
            | not (null exampleOptionalWithNullDefault)
          ],
          [ case exampleSingleOrList of
              [e] -> "single-or-list" JSON..= e
              l -> "single-or-list" JSON..= l
            | not (null exampleSingleOrList)
          ]
        ]

instance FromJSON Example where
  parseJSON = JSON.withObject "Example" $ \o ->
    Example
      <$> o JSON..: "text"
      <*> o JSON..: "bool"
      <*> o JSON..: "maybe"
      <*> o JSON..:? "optional"
      <*> o JSON..:? "optional-or-null"
      <*> o JSON..:? "optional-with-default" JSON..!= "foobar"
      <*> o JSON..:? "optional-with-null-default" JSON..!= []
      <*> ( ((: []) <$> o JSON..: "single-or-list")
              <|> (o JSON..:? "single-or-list" JSON..!= [])
          )
      <*> o JSON..: "fruit"
      <*> o JSON..: "shape"

instance FromMultipart tag Example where
  fromMultipart form =
    Example
      <$> lookupInput "text" form
      <*> ( lookupInput "bool" form >>= \case
              "True" -> Right True
              "False" -> Right False
              _ -> Left "Unknown bool"
          )
      <*> ( lookupInput "maybe" form >>= \case
              "null" -> Right Nothing
              t -> Right (Just t)
          )
      <*> lookupMInput "optional" form
      <*> ( lookupMInput "optional-or-null" form >>= \case
              Nothing -> Right Nothing
              Just "null" -> Right Nothing
              Just t -> Right (Just t)
          )
      <*> (fromMaybe "foobar" <$> lookupMInput "optional-with-default" form)
      <*> lookupLInput "optional-with-null-default" form
      <*> lookupLInput "single-or-list" form
      <*> ( lookupInput "fruit" form >>= \case
              "Apple" -> Right Apple
              "Orange" -> Right Orange
              "Banana" -> Right Banana
              "Melon" -> Right Melon
              _ -> Left "unknown fruit"
          )
      <*> ( lookupInput "shape" form >>= \case
              "circle" -> Right ShapeCircle
              "square" -> Right ShapeSquare
              "rectangle" -> Right ShapeRectangle
              _ -> Left "unknown shape"
          )

instance ToMultipart tag Example where
  toMultipart Example {..} =
    MultipartData
      ( concat
          [ [ Input "text" exampleText,
              Input "bool" $ T.pack $ show exampleBool,
              Input "maybe" $ fromMaybe "null" exampleRequiredMaybe
            ],
            [Input "optional" o | o <- maybeToList exampleOptional],
            [Input "optional-or-null" o | o <- maybeToList exampleOptionalOrNull],
            [Input "optional-with-default" exampleOptionalWithDefault],
            map (Input "optional-with-null-default") exampleOptionalWithNullDefault,
            map (Input "single-or-list") exampleSingleOrList,
            [Input "fruit" $ T.pack $ show exampleFruit],
            [ Input "shape" $ case exampleShape of
                ShapeCircle -> "circle"
                ShapeSquare -> "square"
                ShapeRectangle -> "rectangle"
            ]
          ]
      )
      []

newtype Derived = Derived {unDerived :: Example}
  deriving stock (Show, Eq, Generic)
  deriving newtype (Validity, GenValid, FromJSON, ToJSON)
  deriving newtype (HasCodec) -- This is the important bit.

data ListsExample = ListsExample
  { listsExamplePossiblyEmptyWithOmittedDefault :: [Int],
    listsExamplePossiblyEmptyWithDefault :: [Int],
    listsExampleRequiredNonEmpty :: NonEmpty Text,
    listsExampleOptionalNonEmpty :: Maybe (NonEmpty Text)
  }
  deriving (Show, Eq, Generic)
  deriving
    ( Servant.FromMultipart tag,
      Servant.ToMultipart tag
    )
    via Autodocodec ListsExample
  deriving (OpenAPI.ToSchema) via (AutodocodecOpenApi ListsExample)
  deriving (Swagger.ToSchema) via (AutodocodecSwagger ListsExample)

instance Validity ListsExample

instance NFData ListsExample

instance GenValid ListsExample

instance ToJSON ListsExample where
  toJSON ListsExample {..} =
    JSON.object $
      concat
        [ ["possibly-empty-with-omitted-default" JSON..= listsExamplePossiblyEmptyWithOmittedDefault | listsExamplePossiblyEmptyWithOmittedDefault /= []],
          [ "possibly-empty-with-default" JSON..= listsExamplePossiblyEmptyWithDefault,
            "required-non-empty" JSON..= listsExampleRequiredNonEmpty
          ],
          ["optional-non-empty" JSON..= ne | ne <- maybeToList listsExampleOptionalNonEmpty]
        ]

instance FromJSON ListsExample where
  parseJSON = JSON.withObject "ListsExample" $ \o ->
    ListsExample
      <$> o JSON..:? "possibly-empty-with-omitted-default" JSON..!= []
      <*> o JSON..:? "possibly-empty-with-default" JSON..!= []
      <*> o JSON..: "required-non-empty"
      <*> o JSON..:? "optional-non-empty"

instance HasCodec ListsExample where
  codec = object "ListsExample" objectCodec

instance HasObjectCodec ListsExample where
  objectCodec =
    ListsExample
      <$> optionalFieldWithOmittedDefault "possibly-empty-with-omitted-default" [] "possibly empty list with omitted default empty list" .= listsExamplePossiblyEmptyWithOmittedDefault
      <*> optionalFieldWithDefault "possibly-empty-with-default" [] "possibly empty list with default empty list" .= listsExamplePossiblyEmptyWithDefault
      <*> requiredField "required-non-empty" "required non-empty list" .= listsExampleRequiredNonEmpty
      <*> optionalField "optional-non-empty" "optional non-empty list" .= listsExampleOptionalNonEmpty

-- | A simple Recursive type
--
-- We use this example to make sure that:
--
-- 1. We can define recursive types
-- 2. We can encode, decode, and document a recursive type finitely.
-- 3. We can roundtrip its json schema through json.
data Recursive
  = Base Int
  | Recurse Recursive
  deriving (Show, Eq, Generic)
  deriving (OpenAPI.ToSchema) via AutodocodecOpenApi Recursive
  deriving (Swagger.ToSchema) via AutodocodecSwagger Recursive

instance Validity Recursive

instance NFData Recursive

instance GenValid Recursive where
  shrinkValid = \case
    Base i -> Base <$> shrinkValid i
    Recurse r -> [r]
  genValid = sized $ \n -> case n of
    0 -> Base <$> genValid
    _ ->
      oneof
        [ Base <$> genValid,
          Recurse <$> resize (n - 1) genValid
        ]

instance ToJSON Recursive where
  toJSON = \case
    Base n -> toJSON n
    Recurse r -> JSON.object ["recurse" JSON..= r]

instance FromJSON Recursive where
  parseJSON v =
    JSON.withObject "Recurse" (\o -> Recurse <$> o JSON..: "recurse") v
      <|> (Base <$> JSON.parseJSON v)

instance HasCodec Recursive where
  codec =
    named "recursive" $
      let f = \case
            Left i -> Base i
            Right r -> Recurse r
          g = \case
            Base i -> Left i
            Recurse r -> Right r
       in dimapCodec f g $
            eitherCodec
              (codec @Int <?> "base case")
              (object "Recurse" $ requiredField "recurse" "recursive case")

data MutuallyRecursiveA = MutuallyRecursiveA
  {relationshipToB :: MutuallyRecursiveB}
  deriving stock (Show, Generic, Eq)
  deriving (OpenAPI.ToSchema) via AutodocodecOpenApi MutuallyRecursiveA
  deriving (Swagger.ToSchema) via AutodocodecSwagger MutuallyRecursiveA

instance Validity MutuallyRecursiveA

instance GenValid MutuallyRecursiveA where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance ToJSON MutuallyRecursiveA where
  toJSON MutuallyRecursiveA {..} =
    JSON.object ["relationshipToB" JSON..= relationshipToB]

instance FromJSON MutuallyRecursiveA where
  parseJSON = JSON.withObject "MutuallyRecursiveA" $ \obj ->
    MutuallyRecursiveA <$> obj JSON..: "relationshipToB"

instance HasCodec MutuallyRecursiveA where
  codec =
    named "MutuallyRecursiveA" $
      object "MutuallyRecursiveA" $
        MutuallyRecursiveA <$> requiredField "relationshipToB" "" .= relationshipToB

data MutuallyRecursiveB = MutuallyRecursiveB
  {relationshipToA :: Maybe MutuallyRecursiveA}
  deriving stock (Show, Generic, Eq)

instance Validity MutuallyRecursiveB

instance GenValid MutuallyRecursiveB where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance ToJSON MutuallyRecursiveB where
  toJSON MutuallyRecursiveB {..} =
    JSON.object . filter ((/= JSON.Null) . snd) $ ["relationshipToA" JSON..= relationshipToA]

instance FromJSON MutuallyRecursiveB where
  parseJSON = JSON.withObject "MutuallyRecursiveB" $ \obj ->
    MutuallyRecursiveB <$> obj JSON..:? "relationshipToA"

instance HasCodec MutuallyRecursiveB where
  codec =
    object "MutuallyRecursiveB" $
      MutuallyRecursiveB <$> optionalField "relationshipToA" "" .= relationshipToA

-- | An example of using DerivingVia
data Via = Via
  { viaOne :: !Text,
    viaTwo :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON,
      ToJSON,
      Servant.FromMultipart tag,
      Servant.ToMultipart tag
    )
    via (Autodocodec Via)
  deriving (OpenAPI.ToSchema) via AutodocodecOpenApi Via
  deriving (Swagger.ToSchema) via AutodocodecSwagger Via

instance HasCodec Via where
  codec = object "Via" objectCodec

instance HasObjectCodec Via where
  objectCodec =
    Via
      <$> requiredField "one" "first field" .= viaOne
      <*> requiredField "two" "second field" .= viaTwo

instance Validity Via

instance NFData Via

instance GenValid Via where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

-- | A type with a heavily-commented codec
data VeryComment = VeryComment
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec VeryComment)
  deriving (OpenAPI.ToSchema) via AutodocodecOpenApi VeryComment
  deriving (Swagger.ToSchema) via AutodocodecSwagger VeryComment

instance Validity VeryComment

instance NFData VeryComment

instance GenValid VeryComment where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec VeryComment where
  codec =
    dimapCodec
      (\() -> VeryComment)
      (\VeryComment -> ())
      (nullCodec <?> "This is the inner comment")
      <?> "This is the innermost outer comment"
      <?> "This is the middle outer comment"
      <??> ["This is the outermost outer comment", "on multiple lines", "because we can."]

data LegacyValue = LegacyValue
  { legacyValueText1 :: Text,
    legacyValueText2 :: Text,
    legacyValueText3 :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON,
      ToJSON,
      Servant.FromMultipart tag,
      Servant.ToMultipart tag
    )
    via (Autodocodec LegacyValue)
  deriving (OpenAPI.ToSchema) via AutodocodecOpenApi LegacyValue
  deriving (Swagger.ToSchema) via AutodocodecSwagger LegacyValue

instance Validity LegacyValue

instance NFData LegacyValue

instance GenValid LegacyValue where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec LegacyValue where
  codec = object "LegacyValue" objectCodec

instance HasObjectCodec LegacyValue where
  objectCodec =
    parseAlternative
      ( LegacyValue
          <$> requiredField "1" "text 1" .= legacyValueText1
          <*> requiredField "2" "text 2" .= legacyValueText2
          <*> requiredField "3" "text 3" .= legacyValueText3
      )
      ( LegacyValue
          <$> requiredField "1old" "text 1" .= legacyValueText1
          <*> requiredField "2old" "text 2" .= legacyValueText2
          <*> requiredField "3old" "text 3" .= legacyValueText3
      )

data LegacyObject = LegacyObject
  { legacyObjectText1 :: Text,
    legacyObjectText2 :: Text,
    legacyObjectText3 :: Text,
    legacyObjectWithHistory :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON,
      ToJSON,
      Servant.FromMultipart tag,
      Servant.ToMultipart tag
    )
    via (Autodocodec LegacyObject)
  deriving (OpenAPI.ToSchema) via AutodocodecOpenApi LegacyObject
  deriving (Swagger.ToSchema) via AutodocodecSwagger LegacyObject

instance Validity LegacyObject

instance NFData LegacyObject

instance GenValid LegacyObject where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec LegacyObject where
  codec = object "LegacyObject" objectCodec

instance HasObjectCodec LegacyObject where
  objectCodec =
    LegacyObject
      <$> parseAlternative (requiredField "1" "text 1") (requiredField "1old" "text 1") .= legacyObjectText1
      <*> parseAlternative (requiredField "2" "text 2") (requiredField "2old" "text 2") .= legacyObjectText2
      <*> parseAlternative (requiredField "3" "text 3") (requiredField "3old" "text 3") .= legacyObjectText3
      <*> parseAlternatives
        (requiredField "newest" "newest key")
        [ requiredField "newer" "newer key",
          requiredField "new" "new key",
          requiredField "old" "old key",
          requiredField "older" "older key",
          requiredField "oldest" "oldest key"
        ]
        .= legacyObjectWithHistory

data Ainur
  = Valar !Text !Text
  | Maiar !Text
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec Ainur)
  deriving (OpenAPI.ToSchema) via AutodocodecOpenApi Ainur
  deriving (Swagger.ToSchema) via AutodocodecSwagger Ainur

instance Validity Ainur

instance NFData Ainur

instance GenValid Ainur where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec Ainur where
  codec =
    dimapCodec f g $
      possiblyJointEitherCodec
        ( object "Valar" $
            (,)
              <$> requiredField "domain" "Domain which the Valar rules over" .= fst
              <*> requiredField "name" "Name of the Valar" .= snd
        )
        (object "Maiar" $ requiredField "name" "Name of the Maiar")
    where
      f = \case
        Left (domain, name) -> Valar domain name
        Right name -> Maiar name
      g = \case
        Valar domain name -> Left (domain, name)
        Maiar name -> Right name

data War
  = WorldWar !Word8
  | OtherWar !Text
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec War)
  deriving (OpenAPI.ToSchema) via AutodocodecOpenApi War
  deriving (Swagger.ToSchema) via AutodocodecSwagger War

instance Validity War

instance NFData War

instance GenValid War where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec War where
  codec =
    dimapCodec f g $
      disjointEitherCodec
        (codec :: JSONCodec Word8)
        (codec :: JSONCodec Text)
    where
      f = \case
        Left w -> WorldWar w
        Right t -> OtherWar t
      g = \case
        WorldWar w -> Left w
        OtherWar t -> Right t

data MultilineDefault = MultilineDefault
  { multilineDefaultValue :: !Via -- See above
  }
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON,
      ToJSON
    )
    via (Autodocodec MultilineDefault)
  deriving (OpenAPI.ToSchema) via AutodocodecOpenApi MultilineDefault
  deriving (Swagger.ToSchema) via AutodocodecSwagger MultilineDefault

instance Validity MultilineDefault

instance NFData MultilineDefault

instance GenValid MultilineDefault where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec MultilineDefault where
  codec =
    object "MultilineDefault" $
      MultilineDefault
        <$> optionalFieldWithDefault "value" (Via "foo" "bar") "a field with a multi-line default value" .= multilineDefaultValue

data These
  = This Text
  | That Int
  | Both Text Int
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON,
      ToJSON,
      Servant.FromMultipart tag,
      Servant.ToMultipart tag
    )
    via (Autodocodec These)
  deriving (OpenAPI.ToSchema) via AutodocodecOpenApi These
  deriving (Swagger.ToSchema) via AutodocodecSwagger These

instance Validity These

instance NFData These

instance GenValid These where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec These where
  codec = object "These" objectCodec

instance HasObjectCodec These where
  objectCodec = discriminatedUnionCodec "type" enc dec
    where
      textFieldCodec = requiredField' "text"
      intFieldCodec = requiredField' "int"
      bothFieldsCodec = (,) <$> textFieldCodec .= fst <*> intFieldCodec .= snd
      enc = \case
        This s -> ("this", mapToEncoder s textFieldCodec)
        That n -> ("that", mapToEncoder n intFieldCodec)
        Both s n -> ("both", mapToEncoder (s, n) bothFieldsCodec)
      dec =
        HashMap.fromList
          [ ("this", ("This", mapToDecoder This textFieldCodec)),
            ("that", ("That", mapToDecoder That intFieldCodec)),
            ("both", ("Both", mapToDecoder (uncurry Both) bothFieldsCodec))
          ]

data Expression
  = LiteralExpression Int
  | SumExpression Expression Expression
  | ProductExpression Expression Expression
  deriving stock (Show, Eq, Generic)
  deriving
    ( FromJSON,
      ToJSON,
      Servant.FromMultipart tag,
      Servant.ToMultipart tag
    )
    via (Autodocodec Expression)
  deriving (OpenAPI.ToSchema) via AutodocodecOpenApi Expression
  deriving (Swagger.ToSchema) via AutodocodecSwagger Expression

instance Validity Expression

instance NFData Expression

instance GenValid Expression where
  genValid = sized $ \size ->
    if size > 0
      then
        oneof
          [ LiteralExpression <$> genValid,
            genSplit (pred size) >>= \(size0, size1) ->
              SumExpression <$> resize size0 genValid <*> resize size1 genValid,
            genSplit (pred size) >>= \(size0, size1) ->
              ProductExpression <$> resize size0 genValid <*> resize size1 genValid
          ]
      else LiteralExpression <$> genValid
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec Expression where
  codec = named "Expression" $ object "Expression" objectCodec

instance HasObjectCodec Expression where
  objectCodec = discriminatedUnionCodec "type" enc dec
    where
      valueFieldCodec = requiredField' "value"
      lrFieldsCodec = (,) <$> requiredField' "left" .= fst <*> requiredField' "right" .= snd
      enc = \case
        LiteralExpression n -> ("literal", mapToEncoder n valueFieldCodec)
        SumExpression l r -> ("sum", mapToEncoder (l, r) lrFieldsCodec)
        ProductExpression l r -> ("product", mapToEncoder (l, r) lrFieldsCodec)
      dec =
        HashMap.fromList
          [ ("literal", ("LiteralExpression", mapToDecoder LiteralExpression valueFieldCodec)),
            ("sum", ("SumExpression", mapToDecoder (uncurry SumExpression) lrFieldsCodec)),
            ("product", ("ProductExpression", mapToDecoder (uncurry ProductExpression) lrFieldsCodec))
          ]

identityCodec :: (HasCodec a) => JSONCodec (Identity a)
identityCodec = codec

semigroupFirstCodec :: (HasCodec a) => JSONCodec (Semigroup.First a)
semigroupFirstCodec = codec

semigroupLastCodec :: (HasCodec a) => JSONCodec (Semigroup.Last a)
semigroupLastCodec = codec

monoidFirstCodec :: (HasCodec a) => JSONCodec (Monoid.First a)
monoidFirstCodec = codec

monoidLastCodec :: (HasCodec a) => JSONCodec (Monoid.Last a)
monoidLastCodec = codec

constCodec :: (HasCodec a) => JSONCodec (Const a b)
constCodec = codec

-- Using the non-orphan instances for 'OpenAPI' and 'Swagger' 'ToSchema' instances
newtype NonOrphanExample = NonOrphanExample Example
  deriving newtype (HasCodec)
  deriving (OpenAPI.ToSchema) via (AutodocodecOpenApi NonOrphanExample)
  deriving (Swagger.ToSchema) via (AutodocodecSwagger NonOrphanExample)

data Overlap
  = OverlapA !Text
  | OverlapB !Int
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Autodocodec Overlap
  deriving
    ( Servant.FromMultipart tag,
      Servant.ToMultipart tag
    )
    via Autodocodec Overlap
  deriving (OpenAPI.ToSchema) via (AutodocodecOpenApi Overlap)
  deriving (Swagger.ToSchema) via (AutodocodecSwagger Overlap)

instance Validity Overlap

instance GenValid Overlap where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec Overlap where
  codec =
    dimapCodec f g $
      eitherCodec
        (object "A" (typeField "a" () *> requiredField "text" "text for a"))
        (object "B" (typeField "b" () *> requiredField "int" "int for b"))
    where
      f = \case
        Left s -> OverlapA s
        Right i -> OverlapB i
      g = \case
        OverlapA s -> Left s
        OverlapB i -> Right i

instance HasObjectCodec Overlap where
  objectCodec =
    dimapCodec f g $
      eitherCodec
        (typeField "a" () *> requiredField "string" "string for a")
        (typeField "b" () *> requiredField "int" "int for b")
    where
      f = \case
        Left s -> OverlapA s
        Right i -> OverlapB i
      g = \case
        OverlapA s -> Left s
        OverlapB i -> Right i

typeField :: Text -> a -> ObjectCodec b a
typeField typeName a =
  a <$ requiredFieldWith' "type" (literalTextCodec typeName) .= const typeName
