{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.FormUrlEncodedSpec (spec) where

import Autodocodec
import Autodocodec.FormUrlEncoded
import Autodocodec.Usage
import Data.Bifunctor (first)
import Data.Data
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Utils
import Web.FormUrlEncoded

spec :: Spec
spec = do
  -- Two fields under one key, which is the only shape that tells a
  -- values-concatenating union apart from a left-biased one.
  describe "a key that two fields share" $ do
    let sharedKeyCodec :: ObjectCodec (T.Text, T.Text) (T.Text, T.Text)
        sharedKeyCodec =
          (,)
            <$> requiredField' "k" .= fst
            <*> requiredField' "k" .= snd
    it "encodes the values of both fields" $
      toFormVia sharedKeyCodec ("a", "b")
        `shouldBe` Form (HashMap.singleton "k" ["a", "b"])
    it "encodes the values of both fields in order" $
      toFormVia sharedKeyCodec ("b", "a")
        `shouldBe` Form (HashMap.singleton "k" ["b", "a"])

  describe "decoding errors" $ do
    it "names the key that is missing" $
      (fromFormViaCodec emptyForm :: Either String Via)
        `shouldBe` Left "Failed to parse key \"one\": expected exactly one value, found none."
    it "names the key that has too many values and says how many" $
      ( fromFormViaCodec (Form (HashMap.fromList [("one", ["a", "b"]), ("two", ["c"])])) ::
          Either String Via
      )
        `shouldBe` Left "Failed to parse key \"one\": expected exactly one value, found 2."

  describe "formDecodeSettingEmptyValue" $ do
    -- Both fields present, both empty: one required, one optional.
    let formWithEmptyValues =
          Form $
            HashMap.fromList
              [ ("required-non-empty", [""]),
                ("optional-non-empty", [""])
              ]
    let absentSettings =
          defaultFormDecodeSettings
            { formDecodeSettingEmptyValue = EmptyValueIsAbsent
            }
    it "reads an empty value as a value by default" $
      fromFormViaCodec formWithEmptyValues
        `shouldBe` Right
          ListsExample
            { listsExamplePossiblyEmptyWithOmittedDefault = [],
              listsExamplePossiblyEmptyWithDefault = [],
              listsExampleRequiredNonEmpty = "" :| [],
              listsExampleOptionalNonEmpty = Just ("" :| [])
            }
    it "reads an empty value as absent only for the optional key" $
      fromFormViaCodecWith absentSettings formWithEmptyValues
        `shouldBe` Right
          ListsExample
            { listsExamplePossiblyEmptyWithOmittedDefault = [],
              listsExamplePossiblyEmptyWithDefault = [],
              listsExampleRequiredNonEmpty = "" :| [],
              listsExampleOptionalNonEmpty = Nothing
            }
    it "leaves an empty value among non-empty ones alone" $
      fromFormViaCodecWith
        absentSettings
        ( Form $
            HashMap.fromList
              [ ("required-non-empty", ["a"]),
                ("optional-non-empty", ["", "b"])
              ]
        )
        `shouldBe` Right
          ListsExample
            { listsExamplePossiblyEmptyWithOmittedDefault = [],
              listsExamplePossiblyEmptyWithDefault = [],
              listsExampleRequiredNonEmpty = "a" :| [],
              listsExampleOptionalNonEmpty = Just ("" :| ["b"])
            }
    it "cannot express an optional field that is genuinely the empty string" $
      let example =
            ListsExample
              { listsExamplePossiblyEmptyWithOmittedDefault = [],
                listsExamplePossiblyEmptyWithDefault = [],
                listsExampleRequiredNonEmpty = "a" :| [],
                listsExampleOptionalNonEmpty = Just ("" :| [])
              }
       in fromFormViaCodecWith absentSettings (toFormViaCodec example)
            `shouldBe` Right (example {listsExampleOptionalNonEmpty = Nothing})
    it "falls back to the default of a field that has one" $
      fromFormViaCodecWith
        absentSettings
        ( Form $
            HashMap.fromList
              [ ("required-non-empty", ["a"]),
                ("possibly-empty-with-default", [""])
              ]
        )
        `shouldBe` Right
          ListsExample
            { listsExamplePossiblyEmptyWithOmittedDefault = [],
              listsExamplePossiblyEmptyWithDefault = [],
              listsExampleRequiredNonEmpty = "a" :| [],
              listsExampleOptionalNonEmpty = Nothing
            }
    it "does not change how a missing key reads" $
      fromFormViaCodecWith absentSettings (Form (HashMap.singleton "required-non-empty" ["a"]))
        `shouldBe` Right
          ListsExample
            { listsExamplePossiblyEmptyWithOmittedDefault = [],
              listsExamplePossiblyEmptyWithDefault = [],
              listsExampleRequiredNonEmpty = "a" :| [],
              listsExampleOptionalNonEmpty = Nothing
            }

  formCodecSpec @Example
  formCodecSpec @Via
  formCodecSpec @LegacyValue
  formCodecSpec @LegacyObject
  formCodecSpec @These
  formCodecSpec @Expression
  formCodecSpec @ListsExample
  formCodecSpec @Overlap

formCodecSpec ::
  forall a.
  ( Show a,
    Eq a,
    Typeable a,
    GenValid a,
    ToForm a,
    FromForm a,
    HasObjectCodec a
  ) =>
  Spec
formCodecSpec =
  describe ("formCodecSpec " <> nameOf @a) $ do
    it "matches the encoding" $
      forAllValid $ \(a :: a) ->
        let ctx =
              unlines
                [ "Encoded with this codec",
                  showCodecABit (objectCodec @a)
                ]
            encodedViaInstance = toForm a
            encodedViaCodec = toFormViaCodec a
         in context ctx $ encodedViaCodec `shouldBe` encodedViaInstance
    it "matches the decoding" $
      forAllValid $ \(a :: a) ->
        let encoded = toForm a
            ctx =
              unlines
                [ "Encoded to this value:",
                  ppShow encoded,
                  "with this codec",
                  showCodecABit (objectCodec @a)
                ]
            decodedWithInstance = fromForm encoded :: Either T.Text a
            decodedWithAutodocodec = fromFormViaCodec encoded :: Either String a
         in context ctx $ decodedWithAutodocodec `shouldBe` first T.unpack decodedWithInstance
    codecSpec @a

codecSpec ::
  forall a.
  ( Show a,
    Eq a,
    GenValid a,
    HasObjectCodec a
  ) =>
  Spec
codecSpec = do
  it "roundtrips through Form via the codec" $
    forAllValid $ \(a :: a) ->
      let encoded = toFormViaCodec a
          errOrDecoded = fromFormViaCodec encoded
          ctx =
            unlines
              [ "Encoded to this value:",
                ppShow encoded,
                "with this codec",
                showCodecABit (objectCodec @a)
              ]
       in context ctx $ case errOrDecoded of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` a
  it "roundtrips through the urlencoded wire format via the codec" $
    forAllValid $ \(a :: a) ->
      let encoded = urlEncodeFormStable (toFormViaCodec a)
          errOrDecoded = urlDecodeForm encoded >>= first T.pack . fromFormViaCodec
          ctx =
            unlines
              [ "Encoded to this value:",
                ppShow encoded,
                "with this codec",
                showCodecABit (objectCodec @a)
              ]
       in context ctx $ case errOrDecoded of
            Left err -> expectationFailure (T.unpack err)
            Right actual -> actual `shouldBe` a
