{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-orphans #-}

module Autodocodec.Aeson.Encode
  ( -- * Encoding JSON Values
    toJSONViaCodec,
    toJSONVia,
    toEncodingViaCodec,
    toEncodingVia,

    -- * Encoding JSON Objects
    toJSONObjectViaCodec,
    toJSONObjectVia,
    toSeriesViaCodec,
    toSeriesVia,
  )
where

import qualified Autodocodec.Aeson.Compat as Compat
import Autodocodec.Class
import Autodocodec.Codec
import Autodocodec.DerivingVia
import Data.Aeson (toJSON)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encoding as JSON
import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Scientific
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Implement 'JSON.toJSON' via a type's codec.
toJSONViaCodec :: (HasCodec a) => a -> JSON.Value
toJSONViaCodec = toJSONVia codec

toJSONObjectViaCodec :: (HasObjectCodec a) => a -> JSON.Object
toJSONObjectViaCodec = toJSONObjectVia objectCodec

toJSONObjectVia :: ObjectCodec a void -> a -> JSON.Object
toJSONObjectVia = flip go
  where
    go :: a -> ObjectCodec a void -> JSON.Object
    go a = \case
      RequiredKeyCodec k c _ -> Compat.toKey k JSON..= toJSONVia c a
      OptionalKeyCodec k c _ -> case (coerce a :: Maybe _) of
        Nothing -> mempty
        Just b -> Compat.toKey k JSON..= toJSONVia c b
      OptionalKeyWithDefaultCodec k c _ mdoc -> go (Just a) (optionalKeyCodec k c mdoc)
      OptionalKeyWithOmittedDefaultCodec k c defaultValue mdoc ->
        if coerce a == defaultValue
          then mempty
          else go a (optionalKeyWithDefaultCodec k (coerce c) (coerce defaultValue) mdoc)
      BimapCodec _ g c -> go (g a) c
      PureCodec _ -> mempty
      EitherCodec _ c1 c2 -> case (coerce a :: Either _ _) of
        Left a1 -> go a1 c1
        Right a2 -> go a2 c2
      DiscriminatedUnionCodec propertyName mapping _ ->
        case mapping a of
          (discriminatorValue, c) ->
            Compat.insert (Compat.toKey propertyName) (JSON.String discriminatorValue) $ go a c
      ApCodec oc1 oc2 -> go a oc1 <> go a oc2

-- | Implement 'JSON.toJSON' via a given codec.
toJSONVia :: ValueCodec a void -> a -> JSON.Value
toJSONVia = flip go
  where
    -- We use type-annotations here for readability of type information that is
    -- gathered to case-matching on GADTs, they aren't strictly necessary.
    go :: a -> ValueCodec a void -> JSON.Value
    go a = \case
      NullCodec -> JSON.Null
      BoolCodec _ -> toJSON (coerce a :: Bool)
      StringCodec _ -> toJSON (coerce a :: Text)
      IntegerCodec _ _ -> toJSON (coerce a :: Integer)
      NumberCodec _ _ -> toJSON (coerce a :: Scientific)
      ArrayOfCodec _ c -> toJSON (fmap (`go` c) (coerce a :: Vector _))
      ObjectOfCodec _ oc -> JSON.Object (toJSONObjectVia oc a)
      HashMapCodec c -> Compat.liftToJSON (`go` c) (`go` listCodec c) (coerce a :: HashMap _ _)
      MapCodec c -> Compat.liftToJSON (`go` c) (`go` listCodec c) (coerce a :: Map _ _)
      ValueCodec -> (coerce a :: JSON.Value)
      EqCodec value c -> go value c
      BimapCodec _ g c -> go (g a) c
      EitherCodec _ c1 c2 -> case (coerce a :: Either _ _) of
        Left a1 -> go a1 c1
        Right a2 -> go a2 c2
      CommentCodec _ c -> go a c
      ReferenceCodec _ c -> go a c

-- | Implement 'JSON.toEncoding' via a type's codec.
toEncodingViaCodec :: (HasCodec a) => a -> JSON.Encoding
toEncodingViaCodec = toEncodingVia codec

toSeriesViaCodec :: (HasObjectCodec a) => a -> JSON.Series
toSeriesViaCodec = toSeriesVia objectCodec

toSeriesVia :: ObjectCodec a void -> a -> JSON.Series
toSeriesVia = flip goObject
  where
    goObject :: a -> ObjectCodec a void -> JSON.Series
    goObject a = \case
      RequiredKeyCodec k c _ -> JSON.pair (Compat.toKey k) (toEncodingVia c a)
      OptionalKeyCodec k c _ -> case (coerce a :: Maybe _) of
        Nothing -> mempty :: JSON.Series
        Just b -> JSON.pair (Compat.toKey k) (toEncodingVia c b)
      OptionalKeyWithDefaultCodec k c _ mdoc -> goObject (Just a) (optionalKeyCodec k c mdoc)
      OptionalKeyWithOmittedDefaultCodec k c defaultValue mdoc ->
        if coerce a == defaultValue
          then mempty
          else goObject a (optionalKeyWithDefaultCodec k (coerce c) (coerce defaultValue) mdoc)
      PureCodec _ -> mempty :: JSON.Series
      BimapCodec _ g c -> goObject (g a) c
      EitherCodec _ c1 c2 -> case (coerce a :: Either _ _) of
        Left a1 -> goObject a1 c1
        Right a2 -> goObject a2 c2
      DiscriminatedUnionCodec propertyName mapping _ ->
        case mapping a of
          (discriminatorValue, c) ->
            JSON.pair (Compat.toKey propertyName) (JSON.toEncoding discriminatorValue) <> goObject a c
      ApCodec oc1 oc2 -> goObject a oc1 <> goObject a oc2

-- | Implement 'JSON.toEncoding' via the given codec.
toEncodingVia :: ValueCodec a void -> a -> JSON.Encoding
toEncodingVia = flip go
  where
    go :: a -> ValueCodec a void -> JSON.Encoding
    go a = \case
      NullCodec -> JSON.null_
      BoolCodec _ -> JSON.bool (coerce a :: Bool)
      StringCodec _ -> JSON.text (coerce a :: Text)
      IntegerCodec _ _ -> JSON.scientific (fromInteger (coerce a :: Integer) :: Scientific)
      NumberCodec _ _ -> JSON.scientific (coerce a :: Scientific)
      ArrayOfCodec _ c -> JSON.list (`go` c) (V.toList (coerce a :: Vector _))
      ObjectOfCodec _ oc -> JSON.pairs (toSeriesVia oc a)
      HashMapCodec c -> Compat.liftToEncoding (`go` c) (`go` listCodec c) (coerce a :: HashMap _ _)
      MapCodec c -> Compat.liftToEncoding (`go` c) (`go` listCodec c) (coerce a :: Map _ _)
      ValueCodec -> JSON.value (coerce a :: JSON.Value)
      EqCodec value c -> go value c
      BimapCodec _ g c -> go (g a) c
      EitherCodec _ c1 c2 -> case (coerce a :: Either _ _) of
        Left a1 -> go a1 c1
        Right a2 -> go a2 c2
      CommentCodec _ c -> go a c
      ReferenceCodec _ c -> go a c

instance (HasCodec a) => JSON.ToJSON (Autodocodec a) where
  toJSON = toJSONViaCodec . unAutodocodec
  toEncoding = toEncodingViaCodec . unAutodocodec
