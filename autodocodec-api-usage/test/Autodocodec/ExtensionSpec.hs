{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Autodocodec.ExtensionSpec (spec) where

import Autodocodec
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.GenValidity.Text ()
import Data.Text (Text)
import qualified Data.Text as T
import Test.Syd
import Test.Syd.Validity

-- | A custom "Trees That Grow" phase whose 'XCodec' values are 'Text', with
-- caller-supplied semantics that wrap the text in a reversible @shout:@ prefix.
-- This behaviour is not expressible as a plain codec; it is provided entirely
-- by the handlers passed to the interpreters.
data Shout

type instance XXCodec Shout = ()

type instance XVal Shout = Text

shoutCodec :: Codec Shout JSON.Value Text Text
shoutCodec = XCodec ()

toJSONExt :: ToJSONExt Shout
toJSONExt = ToJSONExt (\() t -> JSON.String ("shout:" <> t))

parseExt :: ParseExt Shout
parseExt = ParseExt $ \() v -> do
  s <- JSON.parseJSON v
  case T.stripPrefix "shout:" s of
    Just t -> pure t
    Nothing -> fail "Autodocodec.ExtensionSpec: expected a \"shout:\" prefix."

spec :: Spec
spec = describe "XCodec extension" $ do
  it "encodes an extension value using the supplied handler" $
    toJSONViaExt toJSONExt shoutCodec "hello" `shouldBe` JSON.String "shout:hello"
  it "roundtrips through toJSONViaExt and parseJSONViaExt" $
    forAllValid $ \t ->
      JSON.parseMaybe
        (parseJSONViaExt parseExt shoutCodec)
        (toJSONViaExt toJSONExt shoutCodec t)
        `shouldBe` Just (t :: Text)
