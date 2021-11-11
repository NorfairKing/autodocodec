{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-orphans #-}

module Autodocodec.Aeson.Encode where

import Autodocodec.Class
import Autodocodec.Codec
import Autodocodec.DerivingVia
import Data.Aeson (toJSON)
import qualified Data.Aeson as JSON
import Data.Scientific
import Data.Text (Text)
import Data.Vector

-- | Encode a value using its JSON codec.
toJSONViaCodec :: HasCodec a => a -> JSON.Value
toJSONViaCodec = toJSONVia codec

-- | Encode a value using a codec for it.
toJSONVia :: ValueCodec a void -> a -> JSON.Value
toJSONVia = toJSONContextVia

-- | Encode a value using a general codec
--
-- You probably won't need this. See 'encodeViaCodec', 'toJSONViaCodec' and 'toJSONVia' instead.
toJSONContextVia :: Codec context a void -> a -> context
toJSONContextVia = flip go
  where
    -- We use type-annotations here for readability of type information that is
    -- gathered to case-matching on GADTs, they aren't strictly necessary.
    go :: a -> Codec context a void -> context
    go a = \case
      NullCodec -> JSON.Null
      BoolCodec _ -> toJSON (a :: Bool)
      StringCodec _ -> toJSON (a :: Text)
      NumberCodec _ -> toJSON (a :: Scientific)
      ArrayOfCodec _ c -> toJSON (fmap (`go` c) (a :: Vector _))
      ObjectOfCodec _ oc -> JSON.Object (go a oc)
      ObjectCodec -> JSON.Object (a :: JSON.Object)
      ValueCodec -> (a :: JSON.Value)
      EqCodec value c -> go value c
      MapCodec _ g c -> go (g a) c
      EitherCodec c1 c2 -> case (a :: Either _ _) of
        Left a1 -> go a1 c1
        Right a2 -> go a2 c2
      CommentCodec _ c -> go a c
      ReferenceCodec _ c -> go a c
      RequiredKeyCodec k c _ -> k JSON..= go a c
      OptionalKeyCodec k c _ -> case (a :: Maybe _) of
        Nothing -> mempty
        Just b -> k JSON..= go b c
      OptionalKeyWithDefaultCodec k c _ mdoc -> go (Just a) (OptionalKeyCodec k c mdoc)
      PureCodec _ -> error "Cannot toJSON a pure object codec."
      ApCodec oc1 oc2 -> go a oc1 <> go a oc2

instance HasCodec a => JSON.ToJSON (Autodocodec a) where
  toJSON = toJSONViaCodec . unAutodocodec
