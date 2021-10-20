{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Aeson.DocumentSpec (spec) where

import Autodocodec
import Autodocodec.Aeson
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.Text (Text)
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = do
  jsonSchemaSpec @Bool "bool"
  jsonSchemaSpec @Text "text"

jsonSchemaSpec :: forall a. (Show a, Eq a, GenValid a, HasCodec a) => FilePath -> Spec
jsonSchemaSpec filePath = do
  it "outputs the same schema as before" $
    pureGoldenJSONValueFile filePath (jsonSchemaViaCodec @a)
