{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

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

    -- * Encoding "Trees That Grow" extension nodes
    ToJSONExt (..),
    toJSONViaExt,
    toJSONObjectViaExt,
    ToEncodingExt (..),
    toEncodingViaExt,
    toSeriesViaExt,
  )
where

import qualified Autodocodec.Aeson.Compat as Compat
import Autodocodec.Class
import Autodocodec.Codec
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

-- | Implement 'JSON.toJSON' via a given codec.
toJSONVia :: ValueCodec a void -> a -> JSON.Value
toJSONVia = toJSONViaExt vanillaToJSONExt

toJSONObjectVia :: ObjectCodec a void -> a -> JSON.Object
toJSONObjectVia = toJSONObjectViaExt vanillaToJSONExt

-- | Implement 'JSON.toEncoding' via a type's codec.
toEncodingViaCodec :: (HasCodec a) => a -> JSON.Encoding
toEncodingViaCodec = toEncodingVia codec

toSeriesViaCodec :: (HasObjectCodec a) => a -> JSON.Series
toSeriesViaCodec = toSeriesVia objectCodec

-- | Implement 'JSON.toEncoding' via the given codec.
toEncodingVia :: ValueCodec a void -> a -> JSON.Encoding
toEncodingVia = toEncodingViaExt vanillaToEncodingExt

toSeriesVia :: ObjectCodec a void -> a -> JSON.Series
toSeriesVia = toSeriesViaExt vanillaToEncodingExt

-- Everything below is for encoding 'Codec's at phases other than 'Vanilla',
-- i.e. codecs that may contain 'XCodec' "Trees That Grow" extension nodes.

-- | How to encode the 'XCodec' extension nodes of a given @phase@ to
-- 'JSON.Value's and 'JSON.Object's.
--
-- An extension node is polymorphic in the codec context, so it may appear both
-- where a value or where an object is expected; hence there is one handler for
-- each. Each handler receives the (typed) extension payload and the value being
-- encoded.
--
-- At the 'Vanilla' phase there are no extension nodes, so 'vanillaToJSONExt'
-- provides handlers that can never be called.
newtype ToJSONExt phase = ToJSONExt
  { -- | Encode an extension node's value (recovered at 'XVal') to a 'JSON.Value'.
    toJSONValueExt :: XXCodec phase -> XVal phase -> JSON.Value
  }

-- | The 'ToJSONExt' for the 'Vanilla' phase; its handler is unreachable.
vanillaToJSONExt :: ToJSONExt Vanilla
vanillaToJSONExt = ToJSONExt (\x _ -> noExtCon x)

-- | Like 'toJSONVia', but for a 'Codec' at any @phase@, given handlers for its
-- extension nodes.
toJSONViaExt :: forall phase a void. ToJSONExt phase -> Codec phase JSON.Value a void -> a -> JSON.Value
toJSONViaExt ext = flip go
  where
    -- We use type-annotations here for readability of type information that is
    -- gathered to case-matching on GADTs, they aren't strictly necessary.
    go :: forall x void'. x -> Codec phase JSON.Value x void' -> JSON.Value
    go a = \case
      NullCodec _ -> JSON.Null
      BoolCodec _ _ -> toJSON (coerce a :: Bool)
      StringCodec _ _ -> toJSON (coerce a :: Text)
      IntegerCodec {} -> toJSON (coerce a :: Integer)
      NumberCodec {} -> toJSON (coerce a :: Scientific)
      ArrayOfCodec _ _ c -> toJSON (fmap (`go` c) (coerce a :: Vector _))
      ObjectOfCodec _ _ oc -> JSON.Object (toJSONObjectViaExt ext oc a)
      HashMapCodec _ c -> Compat.liftToJSON (`go` c) (toJSON . map (`go` c)) (coerce a :: HashMap _ _)
      MapCodec _ c -> Compat.liftToJSON (`go` c) (toJSON . map (`go` c)) (coerce a :: Map _ _)
      ValueCodec _ -> (coerce a :: JSON.Value)
      EqCodec _ value c -> go value c
      BimapCodec _ _ g c -> go (g a) c
      EitherCodec _ _ c1 c2 -> case (coerce a :: Either _ _) of
        Left a1 -> go a1 c1
        Right a2 -> go a2 c2
      CommentCodec _ _ c -> go a c
      ReferenceCodec _ _ c -> go a c
      XCodec meta -> toJSONValueExt ext meta (coerce a)

-- | Like 'toJSONObjectVia', but for a 'Codec' at any @phase@, given handlers
-- for its extension nodes.
toJSONObjectViaExt :: forall phase a void. ToJSONExt phase -> Codec phase JSON.Object a void -> a -> JSON.Object
toJSONObjectViaExt ext = flip go
  where
    go :: forall x void'. x -> Codec phase JSON.Object x void' -> JSON.Object
    go a = \case
      RequiredKeyCodec _ k c _ -> Compat.toKey k JSON..= toJSONViaExt ext c (coerce a)
      OptionalKeyCodec _ k c _ -> case (coerce a :: Maybe _) of
        Nothing -> mempty
        Just b -> Compat.toKey k JSON..= toJSONViaExt ext c b
      OptionalKeyWithDefaultCodec _ k c _ _ -> Compat.toKey k JSON..= toJSONViaExt ext c a
      OptionalKeyWithOmittedDefaultCodec _ k c defaultValue _ ->
        if coerce a == defaultValue
          then mempty
          else Compat.toKey k JSON..= toJSONViaExt ext c (coerce a)
      BimapCodec _ _ g c -> go (g a) c
      PureCodec _ _ -> mempty
      EitherCodec _ _ c1 c2 -> case (coerce a :: Either _ _) of
        Left a1 -> go a1 c1
        Right a2 -> go a2 c2
      DiscriminatedUnionCodec _ propertyName mapping _ ->
        case mapping a of
          (discriminatorValue, c) ->
            Compat.insert (Compat.toKey propertyName) (JSON.String discriminatorValue) $ go a c
      ApCodec _ oc1 oc2 -> go a oc1 <> go a oc2

-- | How to encode the 'XCodec' extension nodes of a given @phase@ to
-- 'JSON.Encoding's and 'JSON.Series'.
--
-- At the 'Vanilla' phase there are no extension nodes, so 'vanillaToEncodingExt'
-- provides handlers that can never be called.
newtype ToEncodingExt phase = ToEncodingExt
  { -- | Encode an extension node's value (recovered at 'XVal') to a 'JSON.Encoding'.
    toEncodingValueExt :: XXCodec phase -> XVal phase -> JSON.Encoding
  }

-- | The 'ToEncodingExt' for the 'Vanilla' phase; its handler is unreachable.
vanillaToEncodingExt :: ToEncodingExt Vanilla
vanillaToEncodingExt = ToEncodingExt (\x _ -> noExtCon x)

-- | Like 'toEncodingVia', but for a 'Codec' at any @phase@, given handlers for
-- its extension nodes.
toEncodingViaExt :: forall phase a void. ToEncodingExt phase -> Codec phase JSON.Value a void -> a -> JSON.Encoding
toEncodingViaExt ext = flip go
  where
    go :: forall x void'. x -> Codec phase JSON.Value x void' -> JSON.Encoding
    go a = \case
      NullCodec _ -> JSON.null_
      BoolCodec _ _ -> JSON.bool (coerce a :: Bool)
      StringCodec _ _ -> JSON.text (coerce a :: Text)
      IntegerCodec {} -> JSON.scientific (fromInteger (coerce a :: Integer) :: Scientific)
      NumberCodec {} -> JSON.scientific (coerce a :: Scientific)
      ArrayOfCodec _ _ c -> JSON.list (`go` c) (V.toList (coerce a :: Vector _))
      ObjectOfCodec _ _ oc -> JSON.pairs (toSeriesViaExt ext oc a)
      HashMapCodec _ c -> Compat.liftToEncoding (`go` c) (JSON.list (`go` c)) (coerce a :: HashMap _ _)
      MapCodec _ c -> Compat.liftToEncoding (`go` c) (JSON.list (`go` c)) (coerce a :: Map _ _)
      ValueCodec _ -> JSON.value (coerce a :: JSON.Value)
      EqCodec _ value c -> go value c
      BimapCodec _ _ g c -> go (g a) c
      EitherCodec _ _ c1 c2 -> case (coerce a :: Either _ _) of
        Left a1 -> go a1 c1
        Right a2 -> go a2 c2
      CommentCodec _ _ c -> go a c
      ReferenceCodec _ _ c -> go a c
      XCodec meta -> toEncodingValueExt ext meta (coerce a)

-- | Like 'toSeriesVia', but for a 'Codec' at any @phase@, given handlers for
-- its extension nodes.
toSeriesViaExt :: forall phase a void. ToEncodingExt phase -> Codec phase JSON.Object a void -> a -> JSON.Series
toSeriesViaExt ext = flip goObject
  where
    goObject :: forall x void'. x -> Codec phase JSON.Object x void' -> JSON.Series
    goObject a = \case
      RequiredKeyCodec _ k c _ -> JSON.pair (Compat.toKey k) (toEncodingViaExt ext c (coerce a))
      OptionalKeyCodec _ k c _ -> case (coerce a :: Maybe _) of
        Nothing -> mempty :: JSON.Series
        Just b -> JSON.pair (Compat.toKey k) (toEncodingViaExt ext c b)
      OptionalKeyWithDefaultCodec _ k c _ _ -> JSON.pair (Compat.toKey k) (toEncodingViaExt ext c a)
      OptionalKeyWithOmittedDefaultCodec _ k c defaultValue _ ->
        if coerce a == defaultValue
          then mempty
          else JSON.pair (Compat.toKey k) (toEncodingViaExt ext c (coerce a))
      PureCodec _ _ -> mempty :: JSON.Series
      BimapCodec _ _ g c -> goObject (g a) c
      EitherCodec _ _ c1 c2 -> case (coerce a :: Either _ _) of
        Left a1 -> goObject a1 c1
        Right a2 -> goObject a2 c2
      DiscriminatedUnionCodec _ propertyName mapping _ ->
        case mapping a of
          (discriminatorValue, c) ->
            JSON.pair (Compat.toKey propertyName) (JSON.toEncoding discriminatorValue) <> goObject a c
      ApCodec _ oc1 oc2 -> goObject a oc1 <> goObject a oc2
