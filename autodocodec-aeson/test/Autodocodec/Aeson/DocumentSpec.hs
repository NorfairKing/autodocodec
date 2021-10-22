{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Aeson.DocumentSpec (spec) where

import Autodocodec
import Autodocodec.Aeson
import Data.Data
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.Text (Text)
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  jsonSchemaSpec @Bool "bool"
  jsonSchemaSpec @Text "text"
  jsonSchemaSpec @(Either Bool Text) "either-bool-text"

jsonSchemaSpec :: forall a. (Show a, Eq a, Typeable a, GenValid a, HasCodec a) => FilePath -> Spec
jsonSchemaSpec filePath = do
  it ("outputs the same schema as before for " <> nameOf @a) $
    pureGoldenJSONValueFile ("test_resources/" <> filePath <> ".json") (jsonSchemaViaCodec @a)
