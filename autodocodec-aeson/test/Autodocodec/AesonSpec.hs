{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.AesonSpec (spec) where

import Autodocodec
import Autodocodec.Aeson
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Data
import Data.GenValidity
import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Data.Int
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Word
import GHC.Generics (Generic)
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  aesonCodecSpec @Bool
  -- Does not hold
  -- aesonCodecSpec @Char
  -- aesonCodecSpec @String
  aesonCodecSpec @Text
  aesonCodecSpec @LT.Text
  aesonCodecSpec @Scientific
  aesonCodecSpec @Int
  aesonCodecSpec @Int8
  aesonCodecSpec @Int16
  aesonCodecSpec @Int32
  aesonCodecSpec @Int64
  aesonCodecSpec @Word
  aesonCodecSpec @Word8
  aesonCodecSpec @Word16
  aesonCodecSpec @Word32
  aesonCodecSpec @Word64
  aesonCodecSpec @(Maybe Text)
  aesonCodecSpec @(Either Text Bool)
  aesonCodecSpec @(Either (Either Text Scientific) Bool)
  aesonCodecSpec @[Text]
  aesonCodecSpec @Fruit
  aesonCodecSpec @Example

data Fruit = Apple | Orange --  | Banana | Melon
  deriving (Show, Eq, Generic)

instance Validity Fruit

instance GenValid Fruit where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec Fruit where
  codec =
    matchChoiceCodec
      ( \case
          Apple -> Just Apple
          _ -> Nothing,
        literalTextValue Apple "Apple"
      )
      ( \case
          Orange -> Just Orange
          _ -> Nothing,
        literalTextValue Orange "Orange"
      )

-- choiceCodec
--   [ literalTextValue Apple "Apple",
--     literalTextValue Orange "Orange",
--     literalTextValue Banana "Banana",
--     literalTextValue Melon "Melon"
--   ]

instance FromJSON Fruit

instance ToJSON Fruit

data Example = Example
  { exampleText :: !Text,
    exampleBool :: !Bool,
    exampleRequiredMaybe :: !(Maybe Text),
    exampleOptional :: !(Maybe Text),
    exampleFruit :: !Fruit
  }
  deriving (Show, Eq, Generic)

instance Validity Example

instance GenValid Example where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec Example where
  codec =
    object "Example" $
      Example
        <$> requiredField "text" .= exampleText
        <*> requiredField "bool" .= exampleBool
        <*> requiredField "maybe" .= exampleRequiredMaybe
        <*> optionalField "optional" .= exampleOptional
        <*> requiredField "fruit" .= exampleFruit

instance ToJSON Example where
  toJSON Example {..} =
    JSON.object
      [ "text" JSON..= exampleText,
        "bool" JSON..= exampleBool,
        "maybe" JSON..= exampleRequiredMaybe,
        "optional" JSON..= exampleOptional,
        "fruit" JSON..= exampleFruit
      ]

instance FromJSON Example where
  parseJSON = JSON.withObject "Example" $ \o ->
    Example
      <$> o JSON..: "text"
      <*> o JSON..: "bool"
      <*> o JSON..: "maybe"
      <*> o JSON..:? "optional"
      <*> o JSON..: "fruit"

aesonCodecSpec :: forall a. (Show a, Eq a, Typeable a, GenValid a, ToJSON a, FromJSON a, HasCodec a) => Spec
aesonCodecSpec = do
  it ("matches the aeson encoding for " <> nameOf @a) $
    forAllValid $ \(a :: a) ->
      let ctx =
            unlines
              [ "Encoded with this codec",
                showCodecABit (codec @a)
              ]
       in context ctx $ toJSONViaCodec a `shouldBe` JSON.toJSON a
  it ("matches the aeson decoding for " <> nameOf @a) $
    forAllValid $ \(a :: a) ->
      let encoded = JSON.toJSON a
          ctx =
            unlines
              [ "Encoded to this value:",
                ppShow encoded,
                "with this codec",
                showCodecABit (codec @a)
              ]
       in context ctx $ JSON.parseEither (parseJSONViaCodec @a) encoded `shouldBe` JSON.parseEither (parseJSON @a) encoded
  codecSpec @a

codecSpec :: forall a. (Show a, Eq a, Typeable a, GenValid a, ToJSON a, HasCodec a) => Spec
codecSpec = do
  it (nameOf @a <> " roundtrips") $
    forAllValid $ \(a :: a) ->
      let encoded = toJSONViaCodec a
          errOrDecoded = JSON.parseEither parseJSONViaCodec encoded
          ctx =
            unlines
              [ "Encoded to this value:",
                ppShow encoded,
                "with this codec",
                showCodecABit (codec @a)
              ]
       in context ctx $ case errOrDecoded of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` a
