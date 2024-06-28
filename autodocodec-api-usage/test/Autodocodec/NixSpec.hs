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
  nixOptionTypeSpec @NullUnit "null"
  nixOptionTypeSpec @Void "void"
  nixOptionTypeSpec @Bool "bool"
  nixOptionTypeSpec @Ordering "ordering"
  nixOptionTypeSpec @Char "char"
  nixOptionTypeSpec @Text "text"
  nixOptionTypeSpec @LT.Text "lazy-text"
  nixOptionTypeSpec @String "string"
  nixOptionTypeSpec @Scientific "scientific"
  nixOptionTypeSpec @JSON.Object "object"
  nixOptionTypeSpec @JSON.Value "value"
  nixOptionTypeSpec @Int "int"
  nixOptionTypeSpec @Int8 "int8"
  nixOptionTypeSpec @Int16 "int16"
  nixOptionTypeSpec @Int32 "int32"
  nixOptionTypeSpec @Int64 "int64"
  nixOptionTypeSpec @Integer "integer"
  nixOptionTypeSpec @Word "word"
  nixOptionTypeSpec @Word8 "word8"
  nixOptionTypeSpec @Word16 "word16"
  nixOptionTypeSpec @Word32 "word32"
  nixOptionTypeSpec @Word64 "word64"
  nixOptionTypeSpec @Natural "natural"
  nixOptionTypeSpec @(Maybe Text) "maybe-text"
  nixOptionTypeSpec @(Either Bool Text) "either-bool-text"
  nixOptionTypeSpec @(Either (Either Bool Scientific) Text) "either-either-bool-scientific-text"
  nixOptionTypeSpec @[Text] "list-text"
  nixOptionTypeSpec @(NonEmpty Text) "nonempty-text"
  nixOptionTypeSpec @(Set Text) "set-text"
  nixOptionTypeSpec @(Map Text Int) "map-text-ind"
  nixOptionTypeSpec @Day "day"
  nixOptionTypeSpec @LocalTime "local-time"
  nixOptionTypeSpec @UTCTime "utc-time"
  nixOptionTypeSpec @TimeOfDay "time-of-day"
  nixOptionTypeSpec @NominalDiffTime "nominal-difftime"
  nixOptionTypeSpec @DiffTime "difftime"
  nixOptionTypeSpec @Fruit "fruit"
  nixOptionTypeSpec @Example "example"
  nixOptionsSpec @Example "example"
  nixOptionTypeSpec @Recursive "recursive"
  nixOptionTypeSpec @ListsExample "lists-example"
  nixOptionsSpec @ListsExample "lists-example"
  nixOptionTypeSpec @MutuallyRecursiveA "mutually-recursive"
  nixOptionTypeSpec @Via "via"
  nixOptionsSpec @Via "via"
  nixOptionTypeSpec @VeryComment "very-comment"
  nixOptionTypeSpec @LegacyValue "legacy-value"
  nixOptionsSpec @LegacyValue "legacy-value"
  nixOptionTypeSpec @LegacyObject "legacy-object"
  nixOptionsSpec @LegacyObject "legacy-object"
  nixOptionTypeSpec @Ainur "ainur"
  nixOptionTypeSpec @War "war"
  nixOptionTypeSpec @These "these"
  nixOptionsSpec @These "these"
  nixOptionTypeSpec @Expression "expression"
  nixOptionsSpec @Expression "expression"

nixOptionsSpec ::
  forall a.
  (HasObjectCodec a) =>
  FilePath ->
  Spec
nixOptionsSpec filePath =
  it "outputs the same nix type as before" $
    pureGoldenTextFile
      ("test_resources/nix/" <> filePath <> "-options.nix")
      (renderNixOptionsViaCodec @a)

nixOptionTypeSpec ::
  forall a.
  (HasCodec a) =>
  FilePath ->
  Spec
nixOptionTypeSpec filePath =
  it "outputs the same nix type as before" $
    pureGoldenTextFile
      ("test_resources/nix/" <> filePath <> "-type.nix")
      (renderNixOptionTypeViaCodec @a)
