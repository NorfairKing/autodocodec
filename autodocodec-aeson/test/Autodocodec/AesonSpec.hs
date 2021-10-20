{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.AesonSpec (spec) where

import Autodocodec
import Autodocodec.Aeson
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.Text (Text)
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  aesonCodecSpec @Bool
  aesonCodecSpec @Text

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
