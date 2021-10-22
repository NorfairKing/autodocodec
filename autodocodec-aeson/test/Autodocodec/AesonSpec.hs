{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.AesonSpec (spec) where

import Autodocodec
import Autodocodec.Aeson
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.GenValidity
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.Scientific
import Data.Text (Text)
import GHC.Generics (Generic)
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  aesonCodecSpec @Bool
  aesonCodecSpec @Text
  aesonCodecSpec @Scientific
  aesonCodecSpec @(Either Text Bool)
  aesonCodecSpec @Example

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

instance ToJSON Example where
  toJSON Example {..} =
    JSON.object
      [ "text" JSON..= exampleText,
        "bool" JSON..= exampleBool
      ]

instance FromJSON Example where
  parseJSON = JSON.withObject "Example" $ \o ->
    Example
      <$> o JSON..: "text"
      <*> o JSON..: "bool"

aesonCodecSpec :: forall a. (Show a, Eq a, GenValid a, ToJSON a, FromJSON a, HasCodec a) => Spec
aesonCodecSpec = do
  it "matches the aeson encoding" $
    forAllValid $ \(a :: a) ->
      toJSONViaCodec a `shouldBe` JSON.toJSON a
  it "matches the aeson decoding" $
    forAllValid $ \(a :: a) ->
      let encoded = JSON.toJSON a
       in JSON.parseEither (parseJSONViaCodec @a) encoded `shouldBe` JSON.parseEither (parseJSON @a) encoded
  codecSpec @a

codecSpec :: forall a. (Show a, Eq a, GenValid a, ToJSON a, HasCodec a) => Spec
codecSpec = do
  it "roundtrips" $
    forAllValid $ \(a :: a) ->
      let encoded = toJSONViaCodec a
          errOrDecoded = JSON.parseEither parseJSONViaCodec encoded
       in case errOrDecoded of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` a
