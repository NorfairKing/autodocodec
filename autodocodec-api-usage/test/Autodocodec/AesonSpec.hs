{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.AesonSpec (spec) where

import Autodocodec
import Autodocodec.Usage
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Data
import Data.GenValidity
import Data.GenValidity.Aeson ()
import Data.GenValidity.Containers ()
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Int
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Scientific
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Void (Void)
import Data.Word
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  aesonCodecSpec @NullUnit
  aesonCodecErrorSpec @NullUnit "null-error-number" "5"
  aesonCodecErrorSpec @Void "void" "{}"
  aesonCodecSpec @Bool
  aesonCodecErrorSpec @Bool "bool-error-string" "\"hello\""
  aesonCodecSpec @Ordering
  aesonCodecErrorSpec @Ordering "ordering-error-list" "[\"foo\"]"
  xdescribe "does not hold" $ aesonCodecSpec @Char
  aesonCodecErrorSpec @Char "char-error-number" "6"
  aesonCodecSpec @Text
  aesonCodecErrorSpec @Text "text-error-bool" "true"
  aesonCodecSpec @LT.Text
  aesonCodecErrorSpec @LT.Text "lazy-text-error-null" "null"
  xdescribe "does not hold" $ aesonCodecSpec @String
  aesonCodecErrorSpec @String "string-error-object" "{}"
  aesonCodecSpec @Scientific
  aesonCodecErrorSpec @Scientific "scientific-error-string" "\"quux\""
  aesonCodecSpec @JSON.Object
  aesonCodecErrorSpec @JSON.Object "object-error-array" "[5, 6]"
  aesonCodecSpec @JSON.Value
  aesonCodecSpec @Int
  aesonCodecErrorSpec @Int "int-error-big" "1E10000"
  aesonCodecSpec @Int8
  aesonCodecErrorSpec @Int8 "int8-error-200" "200"
  aesonCodecSpec @Int16
  aesonCodecSpec @Int32
  aesonCodecSpec @Int64
  aesonCodecSpec @Word
  aesonCodecErrorSpec @Word "word-error-negative" "-3"
  aesonCodecSpec @Word8
  aesonCodecErrorSpec @Word8 "word8-error-300" "300"
  aesonCodecSpec @Word16
  aesonCodecSpec @Word32
  aesonCodecSpec @Word64
  aesonCodecSpec @(Maybe Text)
  aesonCodecErrorSpec @(Maybe Text) "maybe-text-error-object" "{}"
  aesonCodecSpec @(Either Bool Text)
  aesonCodecErrorSpec @(Either Bool Text) "either-bool-text-error-array" "[{}]"
  aesonCodecSpec @(Either (Either Bool Scientific) Text)
  aesonCodecSpec @[Text]
  aesonCodecErrorSpec @[Text] "list-text-error-string" "\"string\""
  aesonCodecSpec @(NonEmpty Text)
  aesonCodecSpec @(Set Text)
  aesonCodecSpec @(Map Text Int)
  aesonCodecSpec @Day
  aesonCodecSpec @LocalTime
  aesonCodecSpec @UTCTime
  aesonCodecSpec @TimeOfDay
  aesonCodecSpec @DiffTime
  aesonCodecSpec @NominalDiffTime
  aesonCodecSpec @Fruit
  aesonCodecSpec @Example
  aesonCodecErrorSpec @Example "example-error-bool-number" "{\"text\": \"hello\", \"bool\": 5}"
  aesonCodecErrorSpec @Example "example-error-fruit-number" "{\"text\": \"hello\", \"bool\": true, \"maybe\": null, \"fruit\": 5}"
  aesonCodecSpec @Recursive
  aesonCodecSpec @ListsExample
  aesonCodecErrorSpec @Recursive "recursive-error-recurse-string" "{\"recurse\": {\"recurse\": {\"recurse\": \"hello\"}}}"
  aesonCodecSpec @MutuallyRecursiveA
  aesonCodecSpec @Via
  aesonCodecSpec @VeryComment
  aesonCodecSpec @LegacyValue
  aesonCodecSpec @LegacyObject
  aesonCodecSpec @Ainur
  aesonCodecSpec @War
  aesonCodecSpec @These
  aesonCodecSpec @Expression

aesonCodecErrorSpec ::
  forall a.
  (HasCodec a) =>
  FilePath ->
  LB.ByteString ->
  Spec
aesonCodecErrorSpec filePath encoded =
  it "shows the same error as before" $
    goldenStringFile ("test_resources/error/" <> filePath <> ".txt") $ do
      let decodedWithAutodocodec = JSON.eitherDecode encoded
      case decodedWithAutodocodec of
        Left err -> pure err
        Right (Autodocodec (_ :: a)) -> expectationFailure "Should not have succeeded."

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

codecSpec ::
  forall a.
  ( Show a,
    Eq a,
    GenValid a,
    ToJSON a,
    HasCodec a
  ) =>
  Spec
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
  it "roundtrips through json bytes" $
    forAllValid $ \(a :: a) ->
      let encoded = encodeJSONViaCodec a
          errOrDecoded = eitherDecodeJSONViaCodec encoded
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
  it "roundtrips through json bytes and back" $
    forAllValid $ \(a :: a) ->
      let encoded = encodeJSONViaCodec a
          errOrDecoded = eitherDecodeJSONViaCodec encoded
          ctx =
            unlines
              [ "Encoded to this value:",
                ppShow encoded,
                "with this codec",
                showCodecABit (codec @a)
              ]
       in context ctx $ case errOrDecoded of
            Left err -> expectationFailure err
            Right actual -> encodeJSONViaCodec (actual :: a) `shouldBe` encodeJSONViaCodec a
