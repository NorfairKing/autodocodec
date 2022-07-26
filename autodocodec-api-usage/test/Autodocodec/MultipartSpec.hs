{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Autodocodec.MultipartSpec (spec) where

import Autodocodec
import Autodocodec.Multipart
import Autodocodec.Usage
import Data.Data
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
  xdescribe "does not hold." $ multipartCodecSpec @Example
  multipartCodecSpec @Via
  multipartCodecSpec @LegacyValue
  multipartCodecSpec @LegacyObject
  multipartCodecSpec @These
  multipartCodecSpec @Expression

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
         in context ctx $ decodedWithAutodocodec `shouldBe` decodedWithAeson
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
  it "roundtrips through MultiPartData Tmp via the codec" $
    forAllValid $ \(a :: a) ->
      let encoded = toMultipartViaCodec a :: MultipartData Tmp
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
