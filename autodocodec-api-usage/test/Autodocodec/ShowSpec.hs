{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.ShowSpec (spec) where

import Autodocodec
import Autodocodec.Usage
import qualified Data.Aeson as JSON
import Data.DList (DList)
import Data.DList.DNonEmpty (DNonEmpty)
import Data.Data
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
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
import Data.Void
import Data.Word
import Numeric.Natural
import Test.Syd
import Test.Syd.Validity.Utils
import Text.Show.Pretty as Pretty

spec :: Spec
spec = do
  showCodecSpec @NullUnit "null"
  showCodecSpec @Void "void"
  showCodecSpec @(Either Void Bool) "either-void-bool"
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
  showCodecSpec @Integer "integer"
  showCodecSpec @Word "word"
  showCodecSpec @Word8 "word8"
  showCodecSpec @Word16 "word16"
  showCodecSpec @Word32 "word32"
  showCodecSpec @Word64 "word64"
  showCodecSpec @Natural "natural"
  showCodecSpec @(Maybe Text) "maybe-text"
  showCodecSpec @(Either Bool Text) "either-bool-text"
  showCodecSpec @(Either (Either Bool Scientific) Text) "either-either-bool-scientific-text"
  showCodecSpec @(Vector Text) "vector-text"
  showCodecSpec @[Text] "list-text"
  showCodecSpec @(DList Text) "dlist-text"
  showCodecSpec @(NonEmpty Text) "nonempty-text"
  showCodecSpec @(DNonEmpty Text) "dnonempty-text"
  showCodecSpec @(Set Text) "set-text"
  showCodecSpec @(Map Text Int) "map-text-int"
  showCodecSpec @Day "day"
  showCodecSpec @LocalTime "local-time"
  showCodecSpec @UTCTime "utc-time"
  showCodecSpec @TimeOfDay "time-of-day"
  showCodecSpec @ZonedTime "zoned-time"
  showCodecSpec @Fruit "fruit"
  showCodecSpec @Shape "shape"
  showCodecSpec @Example "example"
  showCodecSpec @Recursive "recursive"
  showCodecSpec @ListsExample "lists-example"
  showCodecSpec @MutuallyRecursiveA "mutually-recursive"
  showCodecSpec @Via "via"
  showCodecSpec @VeryComment "very-comment"
  showCodecSpec @LegacyValue "legacy-value"
  showCodecSpec @LegacyObject "legacy-object"
  showCodecSpec @Ainur "ainur"
  showCodecSpec @War "war"
  showCodecSpec @These "these"
  showCodecSpec @Expression "expression"
  showCodecSpec @(Identity Text) "identity"
  showCodecSpec @(Dual Text) "dual"
  showCodecSpec @(Semigroup.First Text) "semigroup-first"
  showCodecSpec @(Semigroup.Last Text) "semigroup-last"
  showCodecSpec @(Monoid.First Text) "monoid-first"
  showCodecSpec @(Monoid.Last Text) "monoid-last"
  showCodecSpec @(Const Text Void) "const"

showCodecSpec ::
  forall a.
  ( Typeable a,
    HasCodec a
  ) =>
  FilePath ->
  Spec
showCodecSpec filePath =
  describe ("showCodecSpec " <> nameOf @a)
    $ it "outputs the same shown codec information as before"
    $ pureGoldenStringFile
      ("test_resources/show-codec/" <> filePath <> ".txt")
    $ case Pretty.parseValue (showCodecABit (codec @a)) of
      Nothing -> "Error parsing value"
      Just v -> Pretty.valToStr v
