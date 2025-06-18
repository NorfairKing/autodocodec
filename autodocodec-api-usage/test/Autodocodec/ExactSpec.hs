{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.ExactSpec (spec) where

import Autodocodec
import qualified Autodocodec.Aeson.Compat as Compat
import Autodocodec.Exact
import Autodocodec.Usage
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as JSON
import Data.DList (DList)
import Data.DList.DNonEmpty (DNonEmpty)
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
import Data.Monoid (Dual)
import qualified Data.Monoid as Monoid
import Data.Scientific
import qualified Data.Semigroup as Semigroup
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void)
import Data.Word
import Numeric.Natural
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  exactCodecSpec @NullUnit
  exactCodecErrorSpec @NullUnit "null-error-string" (JSON.String "hi")
  exactCodecErrorSpec @NullUnit "null-error-number" (JSON.Number 5)
  exactCodecSpec @Bool
  exactCodecErrorSpec @Bool "bool-error-string" (JSON.String "hi")
  exactCodecErrorSpec @Bool "bool-error-number" (JSON.Number 5)
  exactCodecSpec @Ordering
  exactCodecErrorSpec @Ordering "ordering-error-string" (JSON.String "hi")
  exactCodecErrorSpec @Ordering "ordering-error-number" (JSON.Number 6)
  xdescribe "does not hold" $ exactCodecSpec @Char
  exactCodecSpec @Text
  exactCodecErrorSpec @Text "text-error-null" JSON.Null
  exactCodecErrorSpec @Text "text-error-number" (JSON.Number 7)
  exactCodecSpec @LT.Text
  xdescribe "does not hold" $ exactCodecSpec @String
  exactCodecSpec @Scientific
  exactCodecErrorSpec @Scientific "scientific-error-null" JSON.Null
  exactCodecErrorSpec @Scientific "scientific-error-string" (JSON.String "hi")
  exactCodecSpec @JSON.Object
  exactCodecSpec @JSON.Value
  exactCodecSpec @Int
  exactCodecErrorSpec @Int "int-error-unsafe" (JSON.Number 1E10000)
  exactCodecSpec @Int8
  exactCodecSpec @Int16
  exactCodecSpec @Int32
  exactCodecSpec @Int64
  exactCodecSpec @Integer
  exactCodecSpec @Word
  exactCodecSpec @Word8
  exactCodecSpec @Word16
  exactCodecSpec @Word32
  exactCodecSpec @Word64
  exactCodecSpec @Natural
  exactCodecSpec @(Maybe Text)
  exactCodecSpec @(Either Bool Text)
  exactCodecSpec @(Either (Either Bool Scientific) Text)
  exactCodecSpec @(Vector Text)
  exactCodecSpec @[Text]
  exactCodecErrorSpec @[Text] "list-text-number" (JSON.Array (Vector.fromList [JSON.String "hi", JSON.Number 5]))
  exactCodecSpec @(DList Text)
  exactCodecSpec @(NonEmpty Text)
  exactCodecErrorSpec @(NonEmpty Text) "non-empty-text-empty" (JSON.Array mempty)
  exactCodecSpec @(DNonEmpty Text)
  exactCodecSpec @(Set Text)
  exactCodecSpec @(Map Text Int)
  exactCodecSpec @Day
  exactCodecSpec @LocalTime
  exactCodecSpec @UTCTime
  exactCodecSpec @TimeOfDay
  exactCodecSpec @DiffTime
  exactCodecSpec @NominalDiffTime
  exactCodecSpec @Fruit
  exactCodecSpec @Shape
  exactCodecSpec @Example
  exactCodecErrorSpec @Example "example-error-string" (JSON.String "hi")
  exactCodecErrorSpec @Example "example-error-missing-required" (JSON.Object (Compat.fromList []))
  exactCodecErrorSpec @Example "example-error-context-field" (JSON.Object (Compat.fromList [("text", JSON.Number 5)]))
  exactCodecSpec @Recursive
  exactCodecErrorSpec @Recursive "recursive-error-long-context" (JSON.Object (Compat.fromList [("recurse", JSON.Object (Compat.fromList [("recurse", JSON.String "hi")]))]))
  exactCodecWarningsSpec @Recursive "recursive-error-unrecognised" (JSON.Object (Compat.fromList [("recurse", JSON.Object (Compat.fromList [("recurse", JSON.Number 5), ("foo", JSON.Null)])), ("bar", JSON.Null), ("baz", JSON.Null)]))
  exactCodecSpec @ListsExample
  exactCodecSpec @MutuallyRecursiveA
  exactCodecSpec @Via
  exactCodecSpec @VeryComment
  exactCodecSpec @LegacyValue
  exactCodecSpec @LegacyObject
  exactCodecSpec @Ainur
  exactCodecSpec @War
  exactCodecSpec @These
  exactCodecErrorSpec @These "these-error-missing-discriminator" (JSON.Object (Compat.fromList [("this", JSON.String "hi")]))
  exactCodecErrorSpec @These "these-error-unknown-discriminator" (JSON.Object (Compat.fromList [("type", JSON.String "huh")]))
  exactCodecErrorSpec @These "these-error-invalid-discriminator" (JSON.Object (Compat.fromList [("type", JSON.Number 5)]))
  exactCodecSpec @Expression
  exactCodecSpec @(Identity Text)
  exactCodecSpec @(Dual Text)
  exactCodecSpec @(Semigroup.First Text)
  exactCodecSpec @(Semigroup.Last Text)
  exactCodecSpec @(Monoid.First Text)
  exactCodecSpec @(Monoid.Last Text)
  exactCodecSpec @(Const Text Void)
  exactCodecSpec @Overlap

exactCodecErrorSpec ::
  forall a.
  (HasCodec a) =>
  FilePath ->
  JSON.Value ->
  Spec
exactCodecErrorSpec filePath encoded =
  it "shows the same error as before" $
    goldenStringFile ("test_resources/exact-error/" <> filePath <> ".txt") $ do
      case parseExactJSONViaCodec encoded of
        Left err -> pure $ prettyExactParseError err
        Right (_ :: a, _) -> expectationFailure "Should not have succeeded."

exactCodecWarningsSpec ::
  forall a.
  (HasCodec a) =>
  FilePath ->
  JSON.Value ->
  Spec
exactCodecWarningsSpec filePath encoded =
  it "shows the same error as before" $
    goldenStringFile ("test_resources/exact-error/" <> filePath <> ".txt") $ do
      case parseExactJSONViaCodec encoded of
        Left err -> expectationFailure $ prettyExactParseError err
        Right (_ :: a, ws) -> case ws of
          [] -> expectationFailure "Should have had warnings."
          _ -> pure $ unlines $ map prettyExactParseWarning ws

exactCodecSpec :: forall a. (Show a, Eq a, GenValid a, ToJSON a, HasCodec a) => Spec
exactCodecSpec = do
  it "roundtrips through json" $
    forAllValid $ \(a :: a) ->
      let encoded = toJSONViaCodec a
          errOrDecoded = parseExactJSONViaCodec encoded
          ctx =
            unlines
              [ "Encoded to this value:",
                ppShow encoded,
                "with this codec",
                showCodecABit (codec @a)
              ]
       in context ctx $ case errOrDecoded of
            Left err -> expectationFailure $ prettyExactParseError err
            Right (actual, warnings) -> do
              actual `shouldBe` a
              warnings `shouldBe` []
  it "roundtrips through json and back" $
    forAllValid $ \(a :: a) ->
      let encoded = toJSONViaCodec a
          errOrDecoded = parseExactJSONViaCodec encoded
          ctx =
            unlines
              [ "Encoded to this value:",
                ppShow encoded,
                "with this codec",
                showCodecABit (codec @a)
              ]
       in context ctx $ case errOrDecoded of
            Left err -> expectationFailure $ prettyExactParseError err
            Right (actual, warnings) -> do
              warnings `shouldBe` []
              JSON.toJSON (actual :: a) `shouldBe` toJSONViaCodec a
