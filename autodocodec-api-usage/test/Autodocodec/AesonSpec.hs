{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.AesonSpec (spec) where

import Autodocodec
import Autodocodec.Aeson
import Autodocodec.Usage
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Data
import Data.GenValidity
import Data.GenValidity.Aeson ()
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.Int
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Word
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  aesonCodecSpec @Bool
  aesonCodecSpec @Ordering
  -- Does not hold
  -- aesonCodecSpec @Char
  -- aesonCodecSpec @String
  aesonCodecSpec @Text
  aesonCodecSpec @LT.Text
  aesonCodecSpec @Scientific
  aesonCodecSpec @JSON.Value
  aesonCodecSpec @Int
  aesonCodecSpec @Int8
  aesonCodecSpec @Int16
  aesonCodecSpec @Int32
  aesonCodecSpec @Int64
  aesonCodecSpec @Word
  aesonCodecSpec @Word8
  aesonCodecSpec @Word16
  aesonCodecSpec @Word32
  aesonCodecSpec @Word64
  aesonCodecSpec @(Maybe Text)
  aesonCodecSpec @(Either Text Bool)
  aesonCodecSpec @(Either (Either Text Scientific) Bool)
  aesonCodecSpec @[Text]
  aesonCodecSpec @Fruit
  aesonCodecSpec @Example
  aesonCodecSpec @Recursive

aesonCodecSpec :: forall a. (Show a, Eq a, Typeable a, GenValid a, ToJSON a, FromJSON a, HasCodec a) => Spec
aesonCodecSpec =
  describe ("aesonCodecSpec " <> nameOf @a) $ do
    it "matches the aeson encoding" $
      forAllValid $ \(a :: a) ->
        let ctx =
              unlines
                [ "Encoded with this codec",
                  showCodecABit (codec @a)
                ]
         in context ctx $ toJSONViaCodec a `shouldBe` JSON.toJSON a
    it "matches the aeson decoding" $
      forAllValid $ \(a :: a) ->
        let encoded = JSON.toJSON a
            ctx =
              unlines
                [ "Encoded to this value:",
                  ppShow encoded,
                  "with this codec",
                  showCodecABit (codec @a)
                ]
            decodedWithAeson = JSON.parseEither (parseJSON @a) encoded
            decodedWithAutodocodec = JSON.parseEither (parseJSONViaCodec @a) encoded
         in context ctx $
              decodedWithAutodocodec `shouldBe` decodedWithAeson
    codecSpec @a

codecSpec :: forall a. (Show a, Eq a, Typeable a, GenValid a, ToJSON a, HasCodec a) => Spec
codecSpec = do
  it "roundtrips through json" $
    forAllValid $ \(a :: a) ->
      let encoded = toJSONViaCodec a
          errOrDecoded = JSON.parseEither parseJSONViaCodec encoded
          ctx =
            unlines
              [ "Encoded to this value:",
                ppShow encoded,
                "with this codec",
                showCodecABit (codec @a)
              ]
       in context ctx $ case errOrDecoded of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` a
  it "roundtrips through json and back" $
    forAllValid $ \(a :: a) ->
      let encoded = toJSONViaCodec a
          errOrDecoded = JSON.parseEither parseJSONViaCodec encoded
          ctx =
            unlines
              [ "Encoded to this value:",
                ppShow encoded,
                "with this codec",
                showCodecABit (codec @a)
              ]
       in context ctx $ case errOrDecoded of
            Left err -> expectationFailure err
            Right actual -> JSON.toJSON (actual :: a) `shouldBe` toJSONViaCodec a
