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
import Test.Syd

spec :: Spec
spec = do
  nixOptionSpec @NullUnit "null"
  nixOptionSpec @Void "void"
  nixOptionSpec @Bool "bool"
  nixOptionSpec @Ordering "ordering"
  nixOptionSpec @Char "char"
  nixOptionSpec @Text "text"
  nixOptionSpec @LT.Text "lazy-text"
  nixOptionSpec @String "string"
  nixOptionSpec @Scientific "scientific"
  nixOptionSpec @JSON.Object "object"
  nixOptionSpec @JSON.Value "value"
  nixOptionSpec @Int "int"
  nixOptionSpec @Int8 "int8"
  nixOptionSpec @Int16 "int16"
  nixOptionSpec @Int32 "int32"
  nixOptionSpec @Int64 "int64"
  nixOptionSpec @Integer "integer"
  nixOptionSpec @Word "word"
  nixOptionSpec @Word8 "word8"
  nixOptionSpec @Word16 "word16"
  nixOptionSpec @Word32 "word32"
  nixOptionSpec @Word64 "word64"
  nixOptionSpec @Natural "natural"
  nixOptionSpec @(Maybe Text) "maybe-text"
  nixOptionSpec @(Either Bool Text) "either-bool-text"
  nixOptionSpec @(Either (Either Bool Scientific) Text) "either-either-bool-scientific-text"
  nixOptionSpec @[Text] "list-text"
  nixOptionSpec @(NonEmpty Text) "nonempty-text"
  nixOptionSpec @(Set Text) "set-text"
  nixOptionSpec @(Map Text Int) "map-text-ind"
  nixOptionSpec @Day "day"
  nixOptionSpec @LocalTime "local-time"
  nixOptionSpec @UTCTime "utc-time"
  nixOptionSpec @TimeOfDay "time-of-day"
  nixOptionSpec @NominalDiffTime "nominal-difftime"
  nixOptionSpec @DiffTime "difftime"
  nixOptionSpec @Fruit "fruit"
  nixOptionSpec @Example "example"
  nixOptionSpec @Recursive "recursive"
  nixOptionSpec @ListsExample "lists-example"
  nixOptionSpec @MutuallyRecursiveA "mutually-recursive"
  nixOptionSpec @Via "via"
  nixOptionSpec @VeryComment "very-comment"
  nixOptionSpec @LegacyValue "legacy-value"
  nixOptionSpec @LegacyObject "legacy-object"
  nixOptionSpec @Ainur "ainur"
  nixOptionSpec @War "war"
  nixOptionSpec @These "these"
  nixOptionSpec @Expression "expression"

nixOptionSpec ::
  forall a.
  (HasCodec a) =>
  FilePath ->
  Spec
nixOptionSpec filePath =
  it "outputs the same nix type as before" $
    pureGoldenTextFile
      ("test_resources/nix/" <> filePath <> ".nix")
      (nixOptionViaCodec @a)
