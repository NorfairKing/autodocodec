{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Autodocodec.Aeson.DocumentSpec (spec) where

import Autodocodec
import Autodocodec.Aeson
import Data.Data
import Data.GenValidity
import Data.GenValidity.Aeson ()
import Data.GenValidity.Containers ()
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.Int
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Word
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  jsonSchemaSpec @Bool "bool"
  jsonSchemaSpec @Ordering "ordering"
  jsonSchemaSpec @Char "char"
  jsonSchemaSpec @Text "text"
  jsonSchemaSpec @LT.Text "lazy-text"
  jsonSchemaSpec @String "string"
  jsonSchemaSpec @Scientific "scientific"
  jsonSchemaSpec @Int "int"
  jsonSchemaSpec @Int8 "int8"
  jsonSchemaSpec @Int16 "int16"
  jsonSchemaSpec @Int32 "int32"
  jsonSchemaSpec @Int64 "int64"
  jsonSchemaSpec @Word "word"
  jsonSchemaSpec @Word8 "word8"
  jsonSchemaSpec @Word16 "word16"
  jsonSchemaSpec @Word32 "word32"
  jsonSchemaSpec @Word64 "word64"
  jsonSchemaSpec @(Maybe Text) "maybe-text"
  jsonSchemaSpec @(Either Bool Text) "either-bool-text"
  jsonSchemaSpec @(Either (Either Bool Scientific) Text) "either-either-bool-scientific-text"
  jsonSchemaSpec @[Text] "list-text"
  jsonSchemaSpec @Example "example"
  describe "JSONSchema" $ do
    genValidSpec @JSONSchema
    jsonSpecOnValid @JSONSchema

instance GenValid JSONSchema where
  shrinkValid = \case
    AnySchema -> []
    NullSchema -> [AnySchema]
    BoolSchema -> [AnySchema]
    StringSchema -> [AnySchema]
    NumberSchema -> [AnySchema]
    ArraySchema s -> s : (ArraySchema <$> shrinkValid s)
    ObjectSchema os -> ObjectSchema <$> shrinkValid os
    ValueSchema v -> ValueSchema <$> shrinkValid v
    ChoiceSchema ss -> case ss of
      [] -> [AnySchema]
      [s] -> [s]
      _ -> ChoiceSchema <$> shrinkValid ss
    CommentSchema k s -> (s :) $ do
      (k', s') <- shrinkValid (k, s)
      pure $ CommentSchema k' s'
  genValid = sized $ \n ->
    if n <= 1
      then elements [AnySchema, NullSchema, BoolSchema, StringSchema, NumberSchema]
      else
        oneof
          [ ArraySchema <$> resize (n -1) genValid,
            ObjectSchema <$> resize (n -1) genValid,
            ChoiceSchema <$> resize (n -1) genValid,
            do
              (a, b) <- genSplit (n -1)
              (CommentSchema <$> resize a genValid <*> resize b genValid) `suchThat` isValid
          ]

instance GenUnchecked KeyRequirement

instance GenValid KeyRequirement

jsonSchemaSpec :: forall a. (Show a, Eq a, Typeable a, GenValid a, HasCodec a) => FilePath -> Spec
jsonSchemaSpec filePath = do
  it ("outputs the same schema as before for " <> nameOf @a) $
    pureGoldenJSONValueFile
      ("test_resources/schema/" <> filePath <> ".json")
      (jsonSchemaViaCodec @a)

data Fruit = Apple | Orange | Banana | Melon
  deriving (Show, Eq, Generic, Enum, Bounded)

instance Validity Fruit

instance GenValid Fruit where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec Fruit where
  codec = shownBoundedEnumCodec

data Example = Example
  { exampleText :: !Text,
    exampleBool :: !Bool,
    exampleRequiredMaybe :: !(Maybe Text),
    exampleOptional :: !(Maybe Text),
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
        <*> requiredField "fruit" .= exampleFruit
