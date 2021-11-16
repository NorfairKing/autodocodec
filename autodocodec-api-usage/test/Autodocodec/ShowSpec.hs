{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.ShowSpec (spec) where

import Autodocodec
import Autodocodec.Usage
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
import Test.Syd
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  showCodecSpec @NullUnit "null"
  showCodecSpec @Bool "bool"
  showCodecSpec @Ordering "ordering"
  showCodecSpec @Char "char"
  showCodecSpec @Text "text"
  showCodecSpec @LT.Text "lazy-text"
  showCodecSpec @String "string"
  showCodecSpec @Scientific "scientific"
  showCodecSpec @JSON.Object "object"
  showCodecSpec @JSON.Value "value"
  showCodecSpec @Int "int"
  showCodecSpec @Int8 "int8"
  showCodecSpec @Int16 "int16"
  showCodecSpec @Int32 "int32"
  showCodecSpec @Int64 "int64"
  showCodecSpec @Word "word"
  showCodecSpec @Word8 "word8"
  showCodecSpec @Word16 "word16"
  showCodecSpec @Word32 "word32"
  showCodecSpec @Word64 "word64"
  showCodecSpec @(Maybe Text) "maybe-text"
  showCodecSpec @(Either Bool Text) "either-bool-text"
  showCodecSpec @(Either (Either Bool Scientific) Text) "either-either-bool-scientific-text"
  showCodecSpec @[Text] "list-text"
  showCodecSpec @(NonEmpty Text) "nonempty-text"
  showCodecSpec @(Set Text) "set-text"
  showCodecSpec @(Map Text Int) "map-text-int"
  showCodecSpec @Day "day"
  showCodecSpec @LocalTime "local-time"
  showCodecSpec @UTCTime "utc-time"
  showCodecSpec @TimeOfDay "time-of-day"
  showCodecSpec @ZonedTime "zoned-time"
  showCodecSpec @Fruit "fruit"
  showCodecSpec @Example "example"
  showCodecSpec @Recursive "recursive"
  showCodecSpec @Via "via"
  showCodecSpec @VeryComment "very-comment"
  showCodecSpec @LegacyValue "legacy-value"
  showCodecSpec @LegacyObject "legacy-object"

showCodecSpec :: forall a. (Typeable a, GenValid a, HasCodec a) => FilePath -> Spec
showCodecSpec filePath =
  describe ("showCodecSpec " <> nameOf @a) $
    it "outputs the same shown codec information as before" $
      pureGoldenStringFile ("test_resources/show-codec/" <> filePath <> ".txt") (showCodecABit (codec @a))
