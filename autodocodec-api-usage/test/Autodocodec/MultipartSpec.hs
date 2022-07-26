{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Autodocodec.MultipartSpec (spec) where

import Autodocodec
import Autodocodec.Multipart
import Autodocodec.Usage
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Data
import Data.Int
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Scientific
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Word
import Servant.Multipart.API
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Utils

deriving instance Show Tmp

deriving instance Show Mem

deriving instance Show (MultipartResult tag) => Show (MultipartData tag)

deriving instance Eq Tmp

deriving instance Eq Mem

deriving instance Eq (MultipartResult tag) => Eq (MultipartData tag)

spec :: Spec
spec = do
  -- multipartCodecSpec @NullUnit
  -- multipartCodecSpec @Bool
  -- multipartCodecSpec @Ordering
  -- xdescribe "does not hold" $ multipartCodecSpec @Char
  -- multipartCodecSpec @Text
  -- multipartCodecSpec @LT.Text
  -- xdescribe "does not hold" $ multipartCodecSpec @String
  -- multipartCodecSpec @Scientific
  -- multipartCodecSpec @JSON.Object
  -- multipartCodecSpec @JSON.Value
  -- multipartCodecSpec @Int
  -- multipartCodecSpec @Int8
  -- multipartCodecSpec @Int16
  -- multipartCodecSpec @Int32
  -- multipartCodecSpec @Int64
  -- multipartCodecSpec @Word
  -- multipartCodecSpec @Word8
  -- multipartCodecSpec @Word16
  -- multipartCodecSpec @Word32
  -- multipartCodecSpec @Word64
  -- multipartCodecSpec @(Maybe Text)
  -- multipartCodecSpec @(Either Bool Text)
  -- multipartCodecSpec @(Either (Either Bool Scientific) Text)
  -- multipartCodecSpec @[Text]
  -- multipartCodecSpec @(NonEmpty Text)
  -- multipartCodecSpec @(Set Text)
  -- multipartCodecSpec @(Map Text Int)
  -- multipartCodecSpec @Day
  -- multipartCodecSpec @LocalTime
  -- multipartCodecSpec @UTCTime
  -- multipartCodecSpec @TimeOfDay
  -- multipartCodecSpec @DiffTime
  -- multipartCodecSpec @NominalDiffTime
  -- multipartCodecSpec @Fruit
  multipartCodecSpec @Example

-- multipartCodecSpec @Recursive
-- multipartCodecSpec @MutuallyRecursiveA
-- multipartCodecSpec @Via
-- multipartCodecSpec @VeryComment
-- multipartCodecSpec @LegacyValue
-- multipartCodecSpec @LegacyObject
-- multipartCodecSpec @Ainur
-- multipartCodecSpec @War
-- multipartCodecSpec @These
-- multipartCodecSpec @Expression

multipartCodecSpec ::
  forall a.
  ( Show a,
    Eq a,
    Typeable a,
    GenValid a,
    ToMultipart Tmp a,
    FromMultipart Tmp a,
    HasObjectCodec a
  ) =>
  Spec
multipartCodecSpec =
  describe ("multipartCodecSpec " <> nameOf @a) $ do
    it "matches the encoding" $
      forAllValid $ \(a :: a) ->
        let ctx =
              unlines
                [ "Encoded with this codec",
                  showCodecABit (objectCodec @a)
                ]
            encodedViaInstance = toMultipart a :: MultipartData Tmp
            encodedViaCodec = toMultipartViaCodec a :: MultipartData Tmp
         in context ctx $ encodedViaCodec `shouldBe` encodedViaInstance
    it "matches the decoding" $
      forAllValid $ \(a :: a) ->
        let encoded = toMultipart a :: MultipartData Tmp
            ctx =
              unlines
                [ "Encoded to this value:",
                  ppShow encoded,
                  "with this codec",
                  showCodecABit (objectCodec @a)
                ]
            decodedWithAeson = fromMultipart encoded :: Either String a
            decodedWithAutodocodec = fromMultipartViaCodec encoded :: Either String a
         in context ctx $
              decodedWithAutodocodec `shouldBe` decodedWithAeson
    codecSpec @a

codecSpec ::
  forall a.
  ( Show a,
    Eq a,
    GenValid a,
    ToMultipart Tmp a,
    FromMultipart Tmp a,
    HasObjectCodec a
  ) =>
  Spec
codecSpec = do
  it "roundtrips through MultiPartData Tmp" $
    forAllValid $ \(a :: a) ->
      let encoded = toMultipartViaCodec a
          errOrDecoded = fromMultipartViaCodec encoded
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
