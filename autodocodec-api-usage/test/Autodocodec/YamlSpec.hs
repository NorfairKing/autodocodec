{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.YamlSpec (spec) where

import Autodocodec
import Autodocodec.Usage
import Autodocodec.Yaml
import qualified Data.Aeson as JSON
import qualified Data.ByteString as SB
import Data.DList (DList)
import Data.DList.DNonEmpty (DNonEmpty)
import Data.Data
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.GenValidity
import Data.GenValidity.Aeson ()
import Data.GenValidity.Containers ()
import Data.GenValidity.DList ()
import Data.GenValidity.DNonEmpty ()
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Int
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Monoid as Monoid
import Data.Scientific
import Data.Semigroup (Dual)
import qualified Data.Semigroup as Semigroup
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Vector (Vector)
import Data.Void (Void)
import Data.Word
import Data.Yaml as Yaml
import Data.Yaml.Builder as YamlBuilder
import Numeric.Natural
import Path
import Path.IO
import Test.QuickCheck
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
  yamlCodecSpec @StringWithBounds
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
  yamlCodecSpec @Integer
  yamlCodecSpec @Word
  yamlCodecSpec @Word8
  yamlCodecSpec @Word16
  yamlCodecSpec @Word32
  yamlCodecSpec @Word64
  yamlCodecSpec @Natural
  yamlCodecSpec @(Maybe Text)
  yamlCodecSpec @(Either Bool Text)
  yamlCodecSpec @(Either (Either Bool [Text]) Text)
  yamlCodecSpec @(Vector Text)
  yamlCodecSpec @[Text]
  yamlCodecSpec @(DList Text)
  yamlCodecSpec @(NonEmpty Text)
  yamlCodecSpec @(DNonEmpty Text)
  yamlCodecSpec @(Set Text)
  yamlCodecSpec @(Map Text Int)
  yamlCodecSpec @Day
  yamlCodecSpec @LocalTime
  yamlCodecSpec @UTCTime
  yamlCodecSpec @TimeOfDay
  yamlCodecSpec @DiffTime
  yamlCodecSpec @NominalDiffTime
  yamlCodecSpec @Fruit
  yamlCodecSpec @Shape
  yamlCodecSpec @Example
  yamlCodecSpec @Derived
  yamlCodecSpec @Recursive
  yamlCodecSpec @MutuallyRecursiveA
  yamlCodecSpec @Via
  yamlCodecSpec @VeryComment
  yamlCodecSpec @LegacyValue
  yamlCodecSpec @LegacyObject
  yamlCodecSpec @Ainur
  yamlCodecSpec @War
  yamlCodecSpec @These
  yamlCodecSpec @Expression
  yamlCodecSpec @(Identity Text)
  yamlCodecSpec @(Dual Text)
  yamlCodecSpec @(Semigroup.First Text)
  yamlCodecSpec @(Semigroup.Last Text)
  yamlCodecSpec @(Monoid.First Text)
  yamlCodecSpec @(Monoid.Last Text)
  yamlCodecSpec @(Const Text Void)
  yamlCodecSpec @Overlap
  yamlCodecSpec @OptionalFields

  describe "readFirstYamlConfigFile" $ do
    it "finds nothing if none of the files exist" $
      withSystemTempDir "autodocodec-test" $ \tdir -> do
        p <- resolveFile tdir "no-such-file.yaml"
        mExample <- readFirstYamlConfigFile [p]
        mExample `shouldBe` (Nothing :: Maybe Example)

    it "reads the first file that exists, not the ones after it" $
      forAllValid $ \example -> ioProperty $
        withSystemTempDir "autodocodec-test" $ \tdir -> do
          missing <- resolveFile tdir "no-such-file.yaml"
          p <- resolveFile tdir "config.yaml"
          later <- resolveFile tdir "later.yaml"
          SB.writeFile (fromAbsFile p) (encodeYamlViaCodec (example :: Example))
          SB.writeFile (fromAbsFile later) "this is not valid yaml for an Example"
          mExample <- readFirstYamlConfigFile [missing, p, later]
          mExample `shouldBe` Just example

  describe "readYamlConfigFile" $
    it "reads back what encodeYamlViaCodec wrote" $
      forAllValid $ \example -> ioProperty $
        withSystemTempDir "autodocodec-test" $ \tdir -> do
          p <- resolveFile tdir "config.yaml"
          SB.writeFile (fromAbsFile p) (encodeYamlViaCodec (example :: Example))
          mExample <- readYamlConfigFile p
          mExample `shouldBe` Just example

yamlCodecSpec ::
  forall a.
  ( Show a,
    Eq a,
    Typeable a,
    GenValid a,
    FromJSON a,
    HasCodec a
  ) =>
  Spec
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
  it "roundtrips through a yaml bytestring" $
    forAllValid $ \(a :: a) ->
      let encoded = encodeYamlViaCodec a
          ctx =
            unlines
              [ "Encoded to this value:",
                ppShow encoded,
                "with this codec",
                showCodecABit (codec @a)
              ]
       in context ctx $
            case eitherDecodeYamlViaCodec encoded of
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
