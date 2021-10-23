{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Aeson.DocumentSpec (spec) where

import Autodocodec
import Autodocodec.Aeson
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
import Test.Syd.Aeson
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  jsonSchemaSpec @Bool "bool"
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

jsonSchemaSpec :: forall a. (Show a, Eq a, Typeable a, GenValid a, HasCodec a) => FilePath -> Spec
jsonSchemaSpec filePath = do
  it ("outputs the same schema as before for " <> nameOf @a) $
    pureGoldenJSONValueFile ("test_resources/schema/" <> filePath <> ".json") (jsonSchemaViaCodec @a)

data Example = Example
  { exampleText :: !Text,
    exampleBool :: !Bool
  }
  deriving (Show, Eq, Generic)

instance Validity Example

instance GenValid Example where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec Example where
  codec =
    object $
      Example
        <$> field "text" .= exampleText
        <*> field "bool" .= exampleBool
