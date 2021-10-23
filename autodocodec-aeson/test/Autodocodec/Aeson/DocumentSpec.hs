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
import Data.Scientific
import Data.Text (Text)
import GHC.Generics (Generic)
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  jsonSchemaSpec @Bool "bool"
  jsonSchemaSpec @Text "text"
  jsonSchemaSpec @Scientific "scientific"
  jsonSchemaSpec @(Either Bool Text) "either-bool-text"
  jsonSchemaSpec @(Either (Either Bool Scientific) Text) "either-either-bool-scientific-text"
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
