{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Yaml.DocumentSpec (spec) where

import Autodocodec
import Autodocodec.Yaml
import Data.Data
import Data.GenValidity
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.Int
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Word
import GHC.Generics (Generic)
import Test.Syd
import Test.Syd.Validity.Utils
import Text.Colour

spec :: Spec
spec = do
  yamlSchemaSpec @Bool "bool"
  yamlSchemaSpec @Ordering "ordering"
  yamlSchemaSpec @Char "char"
  yamlSchemaSpec @Text "text"
  yamlSchemaSpec @LT.Text "lazy-text"
  yamlSchemaSpec @String "string"
  yamlSchemaSpec @Scientific "scientific"
  yamlSchemaSpec @Int "int"
  yamlSchemaSpec @Int8 "int8"
  yamlSchemaSpec @Int16 "int16"
  yamlSchemaSpec @Int32 "int32"
  yamlSchemaSpec @Int64 "int64"
  yamlSchemaSpec @Word "word"
  yamlSchemaSpec @Word8 "word8"
  yamlSchemaSpec @Word16 "word16"
  yamlSchemaSpec @Word32 "word32"
  yamlSchemaSpec @Word64 "word64"
  yamlSchemaSpec @(Maybe Text) "maybe-text"
  yamlSchemaSpec @(Either Bool Text) "either-bool-text"
  yamlSchemaSpec @(Either (Either Bool Scientific) Text) "either-either-bool-scientific-text"
  yamlSchemaSpec @[Text] "list-text"
  yamlSchemaSpec @Fruit "fruit"
  yamlSchemaSpec @Example "example"

data Fruit
  = Apple
  | Orange
  | Banana
  | Melon
  deriving (Show, Eq, Generic, Bounded, Enum)

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

yamlSchemaSpec :: forall a. (Show a, Eq a, Typeable a, GenValid a, HasCodec a) => FilePath -> Spec
yamlSchemaSpec filePath = do
  it ("outputs the same schema as before for " <> nameOf @a) $
    pureGoldenByteStringFile ("test_resources/schema/" <> filePath <> ".txt") (renderChunksBS With24BitColours $ schemaChunksViaCodec @a)
