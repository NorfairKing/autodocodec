{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- This module contains so-called "API Usage" Tests.
-- This means that this module tests that its usage of the API is still supported.
-- Consequentially, we must be careful about refactoring any code in this module.
module Autodocodec.Usage where

import Autodocodec
import Control.Applicative
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import Data.GenValidity
import Data.GenValidity.Aeson ()
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.Maybe
import Data.Text (Text)
import GHC.Generics (Generic)
import Test.QuickCheck

data Fruit
  = Apple
  | Orange
  | Banana
  | Melon
  deriving (Show, Eq, Generic, Enum, Bounded)

instance Validity Fruit

instance GenValid Fruit where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec Fruit where
  codec = shownBoundedEnumCodec

instance FromJSON Fruit

instance ToJSON Fruit

data Example = Example
  { exampleText :: !Text,
    exampleBool :: !Bool,
    exampleRequiredMaybe :: !(Maybe Text),
    exampleOptional :: !(Maybe Text),
    exampleOptionalOrNull :: !(Maybe Text),
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
        <$> requiredField "text" .= exampleText
        <*> requiredField "bool" .= exampleBool
        <*> requiredField "maybe" .= exampleRequiredMaybe
        <*> optionalField "optional" .= exampleOptional
        <*> optionalFieldOrNull "optional-or-null" .= exampleOptionalOrNull
        <*> requiredField "fruit" .= exampleFruit

instance ToJSON Example where
  toJSON Example {..} =
    JSON.object $
      concat
        [ [ "text" JSON..= exampleText,
            "bool" JSON..= exampleBool,
            "maybe" JSON..= exampleRequiredMaybe,
            "fruit" JSON..= exampleFruit
          ],
          [ "optional" JSON..= opt
            | opt <- maybeToList exampleOptional
          ],
          [ "optional-or-null" JSON..= opt
            | opt <- maybeToList exampleOptionalOrNull
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
      <*> o JSON..: "fruit"

-- Recursive type
--
-- We use this example to make sure that:
--
-- 1. We can define recursive types
-- 2. We can encode, decode, and document a recursive type finitely.
-- 3. TODO We can roundtrip its json schema through json.
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
    ReferenceCodec "recursive" $
      let f = \case
            Left i -> Base i
            Right r -> Recurse r
          g = \case
            Base i -> Left i
            Recurse r -> Right r
       in bimapCodec f g $
            eitherCodec
              (codec @Int)
              (object "Recurse" $ requiredField "recurse")
