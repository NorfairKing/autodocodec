{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Autodocodec.CodecSpec (spec) where

import Autodocodec
import Autodocodec.Schema
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Typeable
import Numeric.Natural
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  describe "Union" $ do
    genValidSpec @Union
    eqSpec @Union

  describe "singleOrNonEmptyCodec" $ do
    valueCodecRoundtripSpec @(NonEmpty Text) (singleOrNonEmptyCodec codec)
    it "encodes a one-element list as the element on its own" $
      toJSONVia (singleOrNonEmptyCodec (codec @Text)) ("hi" :| [])
        `shouldBe` JSON.String "hi"
    it "parses an element on its own as a one-element list" $
      JSON.parseMaybe (parseJSONVia (singleOrNonEmptyCodec (codec @Text))) (JSON.String "hi")
        `shouldBe` Just ("hi" :| [])

  describe "stringWithBoundsCodec" $ do
    it "parses a string within the bounds" $
      JSON.parseMaybe
        (parseJSONVia (stringWithBoundsCodec (StringBounds (Just 1) (Just 4))))
        (JSON.String "hi")
        `shouldBe` Just "hi"
    it "does not parse a string longer than the upper bound" $
      JSON.parseMaybe
        (parseJSONVia (stringWithBoundsCodec (StringBounds (Just 1) (Just 4))))
        (JSON.String "hello")
        `shouldBe` (Nothing :: Maybe String)
    it "does not parse a string shorter than the lower bound" $
      JSON.parseMaybe
        (parseJSONVia (stringWithBoundsCodec (StringBounds (Just 2) (Just 4))))
        (JSON.String "a")
        `shouldBe` (Nothing :: Maybe String)
    it "carries its bounds into the schema" $
      jsonSchemaVia (stringWithBoundsCodec (StringBounds (Just 2) (Just 4)))
        `shouldBe` StringSchema (StringBounds (Just 2) (Just 4))

  describe "scientificWithBoundsCodec" $ do
    valueCodecRoundtripSpec @Scientific (scientificWithBoundsCodec emptyBounds)
    it "parses a number within the bounds" $
      JSON.parseMaybe
        (parseJSONVia (scientificWithBoundsCodec (Bounds (Just 2) (Just 4))))
        (JSON.Number 3)
        `shouldBe` Just 3
    it "does not parse a number above the upper bound" $
      JSON.parseMaybe
        (parseJSONVia (scientificWithBoundsCodec (Bounds (Just 2) (Just 4))))
        (JSON.Number 5)
        `shouldBe` (Nothing :: Maybe Scientific)
    it "carries its bounds into the schema" $
      jsonSchemaVia (scientificWithBoundsCodec (Bounds (Just 2) (Just 4)))
        `shouldBe` NumberSchema (Bounds (Just 2) (Just 4))

  describe "unsafeUnboundedIntegerCodec" $ do
    valueCodecRoundtripSpec @Integer unsafeUnboundedIntegerCodec
    it "parses an integer that does not fit in an Int64" $
      JSON.parseMaybe (parseJSONVia unsafeUnboundedIntegerCodec) (JSON.Number 1e20)
        `shouldBe` Just (10 ^ (20 :: Int))
    it "does not parse a number that is not an integer" $
      JSON.parseMaybe (parseJSONVia unsafeUnboundedIntegerCodec) (JSON.Number 1.5)
        `shouldBe` Nothing

  describe "unsafeUnboundedNaturalCodec" $ do
    valueCodecRoundtripSpec @Natural unsafeUnboundedNaturalCodec
    it "parses a natural that does not fit in a Word64" $
      JSON.parseMaybe (parseJSONVia unsafeUnboundedNaturalCodec) (JSON.Number 1e20)
        `shouldBe` Just (10 ^ (20 :: Int))
    it "does not parse a number that is not an integer" $
      JSON.parseMaybe (parseJSONVia unsafeUnboundedNaturalCodec) (JSON.Number 1.5)
        `shouldBe` Nothing
    it "does not parse a negative number" $
      JSON.parseMaybe (parseJSONVia unsafeUnboundedNaturalCodec) (JSON.Number (-1))
        `shouldBe` Nothing

  describe "codecViaAeson" $ do
    valueCodecRoundtripSpec @Ordering (codecViaAeson "Ordering")
    it "encodes what aeson encodes" $
      forAllValid $ \ordering ->
        toJSONVia (codecViaAeson "Ordering") (ordering :: Ordering)
          `shouldBe` JSON.toJSON ordering
    it "names the codec in the schema" $
      jsonSchemaVia (codecViaAeson "Ordering" :: JSONCodec Ordering)
        `shouldBe` CommentSchema "Ordering" AnySchema

  describe "HasCodec (HashMap k v)" $
    it "roundtrips through json" $
      forAllValid $ \m ->
        let hashMap :: HashMap Text Int
            hashMap = HashMap.fromList (M.toList (m :: Map Text Int))
            encoded = toJSONViaCodec hashMap
         in JSON.parseEither parseJSONViaCodec encoded `shouldBe` Right hashMap

  describe "matchChoicesCodec" $ do
    it "encodes with the first codec whose matcher matches" $
      toJSONVia (zeroOrOneOrNumberCodec matchChoicesCodec) 1 `shouldBe` JSON.String "one"
    it "encodes with the fallback codec when no matcher matches" $
      toJSONVia (zeroOrOneOrNumberCodec matchChoicesCodec) 5 `shouldBe` JSON.Number 5
    it "parses what each of its codecs encodes" $
      map
        (JSON.parseMaybe (parseJSONVia (zeroOrOneOrNumberCodec matchChoicesCodec)))
        [JSON.String "zero", JSON.String "one", JSON.Number 5]
        `shouldBe` [Just 0, Just 1, Just 5]
    it "renders an any-of schema" $
      jsonSchemaVia (matchChoicesCodecAs PossiblyJointUnion [(matchOn 0, literalTextValueCodec (0 :: Int) "zero")] codec)
        `shouldBe` AnyOfSchema (ValueSchema (JSON.String "zero") :| [jsonSchemaVia (codec @Int)])

  describe "disjointMatchChoicesCodec" $ do
    it "encodes like matchChoicesCodec does" $
      map
        (toJSONVia (zeroOrOneOrNumberCodec disjointMatchChoicesCodec))
        [0, 1, 5]
        `shouldBe` [JSON.String "zero", JSON.String "one", JSON.Number 5]
    it "renders a one-of schema" $
      jsonSchemaVia (matchChoicesCodecAs DisjointUnion [(matchOn 0, literalTextValueCodec (0 :: Int) "zero")] codec)
        `shouldBe` OneOfSchema (ValueSchema (JSON.String "zero") :| [jsonSchemaVia (codec @Int)])

instance GenValid Union where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

-- | A matcher for 'matchChoicesCodec' that matches exactly one value.
matchOn :: Int -> Int -> Maybe Int
matchOn wanted i = if i == wanted then Just i else Nothing

-- | A codec that spells @0@ and @1@ out as words and renders every other
-- number as a number, built with whichever of the match-choices combinators
-- is given.
zeroOrOneOrNumberCodec ::
  ([(Int -> Maybe Int, JSONCodec Int)] -> JSONCodec Int -> JSONCodec Int) ->
  JSONCodec Int
zeroOrOneOrNumberCodec match =
  match
    [ (matchOn 0, literalTextValueCodec 0 "zero"),
      (matchOn 1, literalTextValueCodec 1 "one")
    ]
    codec

valueCodecRoundtripSpec ::
  forall a.
  (Show a, Eq a, GenValid a, Typeable a) =>
  JSONCodec a ->
  Spec
valueCodecRoundtripSpec c =
  it ("roundtrips through json for " <> nameOf @a) $
    forAllValid $ \(a :: a) ->
      let encoded = toJSONVia c a
       in context (ppShow encoded) $ case JSON.parseEither (parseJSONVia c) encoded of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` a
