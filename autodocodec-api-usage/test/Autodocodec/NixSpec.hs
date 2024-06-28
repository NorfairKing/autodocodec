{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Autodocodec.NixSpec (spec) where

import Autodocodec
import Autodocodec.Nix
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
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Scientific
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Void
import Data.Word
import Numeric.Natural
import Test.QuickCheck
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  nixTypeSpec @NullUnit "null"
  nixTypeSpec @Void "void"
  nixTypeSpec @Bool "bool"
  nixTypeSpec @Ordering "ordering"
  nixTypeSpec @Char "char"
  nixTypeSpec @Text "text"
  nixTypeSpec @LT.Text "lazy-text"
  nixTypeSpec @String "string"
  nixTypeSpec @Scientific "scientific"
  nixTypeSpec @JSON.Object "object"
  nixTypeSpec @JSON.Value "value"
  nixTypeSpec @Int "int"
  nixTypeSpec @Int8 "int8"
  nixTypeSpec @Int16 "int16"
  nixTypeSpec @Int32 "int32"
  nixTypeSpec @Int64 "int64"
  nixTypeSpec @Integer "integer"
  nixTypeSpec @Word "word"
  nixTypeSpec @Word8 "word8"
  nixTypeSpec @Word16 "word16"
  nixTypeSpec @Word32 "word32"
  nixTypeSpec @Word64 "word64"
  nixTypeSpec @Natural "natural"
  nixTypeSpec @(Maybe Text) "maybe-text"
  nixTypeSpec @(Either Bool Text) "either-bool-text"
  nixTypeSpec @(Either (Either Bool Scientific) Text) "either-either-bool-scientific-text"
  nixTypeSpec @[Text] "list-text"
  nixTypeSpec @(NonEmpty Text) "nonempty-text"
  nixTypeSpec @(Set Text) "set-text"
  nixTypeSpec @(Map Text Int) "map-text-ind"
  nixTypeSpec @Day "day"
  nixTypeSpec @LocalTime "local-time"
  nixTypeSpec @UTCTime "utc-time"
  nixTypeSpec @TimeOfDay "time-of-day"
  nixTypeSpec @NominalDiffTime "nominal-difftime"
  nixTypeSpec @DiffTime "difftime"
  nixTypeSpec @Fruit "fruit"
  nixTypeSpec @Example "example"
  nixTypeSpec @Recursive "recursive"
  nixTypeSpec @ListsExample "lists-example"
  nixTypeSpec @MutuallyRecursiveA "mutually-recursive"
  nixTypeSpec @Via "via"
  nixTypeSpec @VeryComment "very-comment"
  nixTypeSpec @LegacyValue "legacy-value"
  nixTypeSpec @LegacyObject "legacy-object"
  nixTypeSpec @Ainur "ainur"
  nixTypeSpec @War "war"
  nixTypeSpec @These "these"
  nixTypeSpec @Expression "expression"

nixTypeSpec ::
  forall a.
  ( Show a,
    Typeable a,
    HasCodec a
  ) =>
  FilePath ->
  Spec
nixTypeSpec filePath =
  it "outputs the same nix type as before" $
    pureGoldenJSONFile
      ("test_resources/nix/" <> filePath <> ".nix")
      (JSON.toJSON (nixTypeViaCodec @a))
