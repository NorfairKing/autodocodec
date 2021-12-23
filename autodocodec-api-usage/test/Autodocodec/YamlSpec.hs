{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.YamlSpec (spec) where

import Autodocodec
import Autodocodec.Usage
import Autodocodec.Yaml.Encode
import qualified Data.Aeson as JSON
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
import Data.Word
import Data.Yaml as Yaml
import Data.Yaml.Builder as YamlBuilder
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  yamlCodecSpec @NullUnit
  yamlCodecSpec @Bool
  yamlCodecSpec @Ordering
  xdescribe "does not hold" $ yamlCodecSpec @Char
  yamlCodecSpec @Text
  yamlCodecSpec @LT.Text
  xdescribe "does not hold" $ yamlCodecSpec @String
  xdescribe "does not hold" $ yamlCodecSpec @Scientific
  xdescribe "does not hold" $ yamlCodecSpec @JSON.Object
  xdescribe "does not hold" $ yamlCodecSpec @JSON.Value
  yamlCodecSpec @Int
  yamlCodecSpec @Int8
  yamlCodecSpec @Int16
  yamlCodecSpec @Int32
  yamlCodecSpec @Int64
  yamlCodecSpec @Word
  yamlCodecSpec @Word8
  yamlCodecSpec @Word16
  yamlCodecSpec @Word32
  yamlCodecSpec @Word64
  yamlCodecSpec @(Maybe Text)
  yamlCodecSpec @(Either Bool Text)
  yamlCodecSpec @(Either (Either Bool [Text]) Text)
  yamlCodecSpec @[Text]
  yamlCodecSpec @(NonEmpty Text)
  yamlCodecSpec @(Set Text)
  yamlCodecSpec @(Map Text Int)
  yamlCodecSpec @Day
  yamlCodecSpec @LocalTime
  yamlCodecSpec @UTCTime
  yamlCodecSpec @TimeOfDay
  yamlCodecSpec @DiffTime
  yamlCodecSpec @NominalDiffTime
  yamlCodecSpec @Fruit
  yamlCodecSpec @Example
  yamlCodecSpec @Recursive
  yamlCodecSpec @Via
  yamlCodecSpec @VeryComment
  yamlCodecSpec @LegacyValue
  yamlCodecSpec @LegacyObject
  yamlCodecSpec @Ainur
  yamlCodecSpec @War

yamlCodecSpec :: forall a. (Show a, Eq a, Typeable a, GenValid a, ToJSON a, FromJSON a, HasCodec a) => Spec
yamlCodecSpec = describe (nameOf @a) $ do
  it "roundtrips through yaml" $
    forAllValid $ \(a :: a) ->
      let encoded = YamlBuilder.toByteString (toYamlViaCodec a)
          errOrDecoded = Yaml.decodeEither' encoded
          ctx =
            unlines
              [ "Encoded to this value:",
                ppShow encoded,
                "with this codec",
                showCodecABit (codec @a)
              ]
       in context ctx $
            case errOrDecoded of
              Left err -> expectationFailure $ Yaml.prettyPrintParseException err
              Right actual -> actual `shouldBe` a
  it "roundtrips through yaml and back" $
    forAllValid $ \(a :: a) ->
      let encoded = YamlBuilder.toByteString (toYamlViaCodec a)
          errOrDecoded = Yaml.decodeEither' encoded
          ctx =
            unlines
              [ "Encoded to this value:",
                ppShow encoded,
                "with this codec",
                showCodecABit (codec @a)
              ]
       in context ctx $ case errOrDecoded of
            Left err -> expectationFailure $ Yaml.prettyPrintParseException err
            Right actual -> YamlBuilder.toByteString (toYamlViaCodec (actual :: a)) `shouldBe` YamlBuilder.toByteString (toYamlViaCodec a)
