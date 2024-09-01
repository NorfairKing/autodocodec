{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Autodocodec
import Autodocodec.Usage
import Control.DeepSeq
import Control.Monad
import Criterion.Main as Criterion
import qualified Data.Aeson as Aeson
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity.Containers ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Int
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Typeable
import Data.Vector (Vector)
import Data.Word
import Numeric.Natural
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Test.Syd.Validity
import Test.Syd.Validity.Utils

main :: IO ()
main =
  Criterion.defaultMain
    [ comparisonBench @NullUnit,
      comparisonBench @Bool,
      comparisonBench @Ordering,
      comparisonBench @Text,
      comparisonBench @LT.Text,
      -- Do not roundtrip
      -- comparisonBench @Char,
      -- comparisonBench @String,
      -- comparisonBench @Scientific,
      -- comparisonBench @JSON.Object,
      -- comparisonBench @JSON.Value,
      comparisonBench @Int,
      comparisonBench @Int8,
      comparisonBench @Int16,
      comparisonBench @Int32,
      comparisonBench @Int64,
      comparisonBench @Integer,
      comparisonBench @Word,
      comparisonBench @Word8,
      comparisonBench @Word16,
      comparisonBench @Word32,
      comparisonBench @Word64,
      comparisonBench @Natural,
      comparisonBench @(Maybe Text),
      comparisonBench @(Either Bool Text),
      comparisonBench @(Either (Either Bool [Text]) Text),
      comparisonBench @(Vector Text),
      comparisonBench @[Text],
      comparisonBench @(NonEmpty Text),
      comparisonBench @(Set Text),
      comparisonBench @(Map Text Int),
      comparisonBench @Day,
      comparisonBench @LocalTime,
      comparisonBench @UTCTime,
      comparisonBench @TimeOfDay,
      comparisonBench @DiffTime,
      comparisonBench @NominalDiffTime,
      comparisonBench @Fruit,
      comparisonBench @Shape,
      comparisonBench @Example,
      comparisonBench @Recursive,
      comparisonBench @Via,
      comparisonBench @VeryComment,
      comparisonBench @LegacyValue,
      comparisonBench @LegacyObject
    ]

comparisonBench ::
  forall a.
  ( GenValid a,
    Typeable a,
    HasCodec a,
    Aeson.FromJSON a,
    Aeson.ToJSON a,
    NFData a
  ) =>
  Benchmark
comparisonBench =
  let genAs =
        pure (unGen (replicateM 100 genValid) (mkQCGen 42) 30 :: [a])
   in env genAs $ \as ->
        bgroup
          (nameOf @a)
          [ bgroup
              "encoding"
              [ bgroup
                  "toJSON"
                  [ bench "toJSON (Autodocodec)" $
                      nf toJSONViaCodec as,
                    bench "toJSON (Aeson)" $
                      nf Aeson.toJSON as
                  ],
                bgroup
                  "toByteString"
                  [ bench "encode (Autodocodec)" $
                      nf encodeJSONViaCodec as,
                    bench "encode (Aeson)" $
                      nf Aeson.encode as
                  ]
              ],
            let genBS = pure $ LB.fromStrict . LB.toStrict . Aeson.encode $ as
             in env genBS $ \bs ->
                  bgroup
                    "decoding"
                    [ let decodeToValue = pure $ fromJust $ Aeson.decode bs
                       in env decodeToValue $ \val ->
                            bgroup
                              "parseJSON"
                              [ bench "parseJSON (Autodocodec)" $
                                  nf (Aeson.parseMaybe parseJSONViaCodec :: JSON.Value -> Maybe a) val,
                                bench "parseJSON (Aeson)" $
                                  nf (Aeson.parseMaybe Aeson.parseJSON :: JSON.Value -> Maybe a) val
                              ],
                      bgroup
                        "parseByteString"
                        [ bench "decode (Autodocodec)" $
                            nf (eitherDecodeJSONViaCodec :: LB.ByteString -> Either String a) bs,
                          bench "decode (Aeson)" $
                            nf (Aeson.eitherDecode :: LB.ByteString -> Either String a) bs
                        ]
                    ]
          ]
