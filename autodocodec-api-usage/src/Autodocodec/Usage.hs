{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
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
import Autodocodec.OpenAPI ()
import Autodocodec.Swagger ()
import Control.Applicative
import Control.DeepSeq
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import Data.GenValidity
import Data.GenValidity.Aeson ()
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.OpenApi (ToSchema)
import qualified Data.OpenApi as OpenAPI
import qualified Data.Swagger as Swagger
import Data.Text (Text)
import Data.Word
import GHC.Generics (Generic)
import Test.QuickCheck

-- | A type that's encoded as @null@.
data NullUnit = NullUnit
  deriving (Show, Eq, Generic)
  deriving (OpenAPI.ToSchema) via Autodocodec NullUnit

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
  deriving (OpenAPI.ToSchema) via Autodocodec Fruit

instance Validity Fruit

instance NFData Fruit

instance GenValid Fruit where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec Fruit where
  codec = shownBoundedEnumCodec

instance FromJSON Fruit

instance ToJSON Fruit

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
    exampleFruit :: !Fruit
  }
  deriving (Show, Eq, Generic)
  deriving (OpenAPI.ToSchema) via Autodocodec Example

instance Validity Example

instance NFData Example

instance GenValid Example where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec Example where
  codec =
    object "Example" $
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

instance ToJSON Example where
  toJSON Example {..} =
    JSON.object $
      concat
        [ [ "text" JSON..= exampleText,
            "bool" JSON..= exampleBool,
            "maybe" JSON..= exampleRequiredMaybe,
            "fruit" JSON..= exampleFruit,
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
  deriving (ToSchema) via Autodocodec Recursive

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
  deriving (ToSchema) via Autodocodec MutuallyRecursiveA

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
      Swagger.ToSchema,
      OpenAPI.ToSchema
    )
    via (Autodocodec Via)

instance HasCodec Via where
  codec =
    object "Via" $
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
      ToJSON,
      Swagger.ToSchema,
      OpenAPI.ToSchema
    )
    via (Autodocodec VeryComment)

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
      Swagger.ToSchema,
      OpenAPI.ToSchema
    )
    via (Autodocodec LegacyValue)

instance Validity LegacyValue

instance NFData LegacyValue

instance GenValid LegacyValue where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec LegacyValue where
  codec =
    parseAlternatives
      ( object "LegacyValue" $
          LegacyValue
            <$> requiredField "1" "text 1" .= legacyValueText1
            <*> requiredField "2" "text 2" .= legacyValueText2
            <*> requiredField "3" "text 3" .= legacyValueText3
      )
      [ object "LegacyValueOld" $
          LegacyValue
            <$> requiredField "1old" "text 1" .= legacyValueText1
            <*> requiredField "2old" "text 2" .= legacyValueText2
            <*> requiredField "3old" "text 3" .= legacyValueText3
      ]

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
      Swagger.ToSchema,
      OpenAPI.ToSchema
    )
    via (Autodocodec LegacyObject)

instance Validity LegacyObject

instance NFData LegacyObject

instance GenValid LegacyObject where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec LegacyObject where
  codec =
    object "LegacyObject" $
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
      ToJSON,
      Swagger.ToSchema,
      OpenAPI.ToSchema
    )
    via (Autodocodec Ainur)

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
      ToJSON,
      Swagger.ToSchema,
      OpenAPI.ToSchema
    )
    via (Autodocodec War)

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
      ToJSON,
      Swagger.ToSchema,
      OpenAPI.ToSchema
    )
    via (Autodocodec MultilineDefault)

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
      Swagger.ToSchema,
      OpenAPI.ToSchema
    )
    via (Autodocodec These)

instance Validity These

instance NFData These

instance GenValid These where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec These where
  codec =
    object "These" $
      discriminatedUnionCodec "type" enc dec
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
      Swagger.ToSchema,
      OpenAPI.ToSchema
    )
    via (Autodocodec Expression)

instance Validity Expression where
  validate = trivialValidation

instance NFData Expression

instance GenValid Expression where
  genValid = sized $ \size -> do
    if size > 0
      then
        oneof
          [ LiteralExpression <$> genValid,
            SumExpression <$> scale (`div` 2) genValid <*> scale (`div` 2) genValid,
            ProductExpression <$> scale (`div` 2) genValid <*> scale (`div` 2) genValid
          ]
      else
        LiteralExpression <$> genValid
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec Expression where
  codec =
    named "Expression" $ object "Expression" $ discriminatedUnionCodec "type" enc dec
    where
      valueFieldCodec = requiredField' "value"
      lrFieldsCodec = (,) <$> requiredField' "left" .= fst <*> requiredField' "right" .= snd
      enc = \case
        LiteralExpression n -> ("literal", mapToEncoder n valueFieldCodec)
        SumExpression l r -> ("sum", mapToEncoder (l, r) lrFieldsCodec)
        ProductExpression l r -> ("product", mapToEncoder (l, r) lrFieldsCodec)
      dec =
        HashMap.fromList
          [ ("literal", ("LiteralExpression",  mapToDecoder LiteralExpression valueFieldCodec)),
            ("sum", ("SumExpression", mapToDecoder (uncurry SumExpression) lrFieldsCodec)),
            ("product", ("ProductExpression", mapToDecoder (uncurry ProductExpression) lrFieldsCodec))
          ]
