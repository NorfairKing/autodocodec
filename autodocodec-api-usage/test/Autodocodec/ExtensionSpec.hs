{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Autodocodec.ExtensionSpec (spec) where

import Autodocodec
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as JSON
import Data.GenValidity.Text ()
import Data.Text (Text)
import qualified Data.Text as T
import Test.Syd
import Test.Syd.Validity

-- | A custom "Trees That Grow" phase with both a value-context ('XCodec') and
-- an object-context ('XObjectCodec') extension, each carrying 'Text' with
-- caller-supplied semantics that no plain codec expresses:
--
-- * the value extension wraps the text in a reversible @shout:@ prefix, and
-- * the object extension stores the text under a @wrapped@ key.
data Ext

type instance XXCodec Ext = ()

type instance XVal Ext = Text

type instance XXObjectCodec Ext = ()

type instance XObjVal Ext = Text

valueExtCodec :: Codec Ext JSON.Value Text Text
valueExtCodec = XCodec ()

objectExtCodec :: Codec Ext JSON.Object Text Text
objectExtCodec = XObjectCodec ()

toJSONExt :: ToJSONExt Ext
toJSONExt =
  ToJSONExt
    (\() t -> JSON.String ("shout:" <> t))
    (\() t -> KM.singleton "wrapped" (JSON.String t))

parseExt :: ParseExt Ext
parseExt =
  ParseExt
    ( \() v -> do
        s <- JSON.parseJSON v
        case T.stripPrefix "shout:" s of
          Just t -> pure t
          Nothing -> fail "Autodocodec.ExtensionSpec: expected a \"shout:\" prefix."
    )
    (\() o -> o JSON..: "wrapped")

spec :: Spec
spec = describe "extension codecs" $ do
  describe "value context (XCodec)" $ do
    it "encodes using the supplied handler" $
      toJSONViaExt toJSONExt valueExtCodec "hello" `shouldBe` JSON.String "shout:hello"
    it "roundtrips through toJSONViaExt and parseJSONViaExt" $
      forAllValid $ \t ->
        JSON.parseMaybe
          (parseJSONViaExt parseExt valueExtCodec)
          (toJSONViaExt toJSONExt valueExtCodec t)
          `shouldBe` Just (t :: Text)
  describe "object context (XObjectCodec)" $ do
    it "encodes using the supplied handler" $
      toJSONObjectViaExt toJSONExt objectExtCodec "hello"
        `shouldBe` KM.singleton "wrapped" (JSON.String "hello")
    it "roundtrips through toJSONObjectViaExt and parseJSONObjectViaExt" $
      forAllValid $ \t ->
        JSON.parseMaybe
          (parseJSONObjectViaExt parseExt objectExtCodec)
          (toJSONObjectViaExt toJSONExt objectExtCodec t)
          `shouldBe` Just (t :: Text)
