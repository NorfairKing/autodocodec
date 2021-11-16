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
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import Data.GenValidity
import Data.GenValidity.Aeson ()
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.Maybe
import qualified Data.OpenApi as OpenAPI
import qualified Data.Swagger as Swagger
import Data.Text (Text)
import GHC.Generics (Generic)
import Test.QuickCheck

-- | A type that's encoded as @null@.
data NullUnit = NullUnit
  deriving (Show, Eq, Generic)

instance Validity NullUnit

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

instance Validity Fruit

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
    exampleFruit :: !Fruit
  }
  deriving (Show, Eq, Generic)

instance Validity Example

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

instance Validity Recursive

instance GenValid Recursive where
  shrinkValid = \case
    Base i -> Base <$> shrinkValid i
    Recurse r -> [r]
  genValid = sized $ \n -> case n of
    0 -> Base <$> genValid
    _ ->
      oneof
        [ Base <$> genValid,
          Recurse <$> resize (n -1) genValid
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
      [ object "LegacyValue" $
          LegacyValue
            <$> requiredField "1old" "text 1" .= legacyValueText1
            <*> requiredField "2old" "text 2" .= legacyValueText2
            <*> requiredField "3old" "text 3" .= legacyValueText3
      ]

data LegacyObject = LegacyObject
  { legacyObjectText1 :: Text,
    legacyObjectText2 :: Text,
    legacyObjectText3 :: Text
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

instance GenValid LegacyObject where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance HasCodec LegacyObject where
  codec =
    object "LegacyObject" $
      LegacyObject
        <$> parseAlternative (requiredField "1" "text 1") (requiredField "1old" "text 1") .= legacyObjectText1
        <*> parseAlternative (requiredField "2" "text 2") (requiredField "2old" "text 2") .= legacyObjectText1
        <*> parseAlternative (requiredField "3" "text 3") (requiredField "3old" "text 3") .= legacyObjectText1
