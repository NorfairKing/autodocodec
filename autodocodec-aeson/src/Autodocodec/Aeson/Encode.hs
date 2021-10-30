{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Autodocodec.Aeson.Encode where

import Autodocodec
import Data.Aeson (toJSON)
import qualified Data.Aeson as JSON
import Data.Scientific
import Data.Text (Text)

toJSONViaCodec :: HasCodec a => a -> JSON.Value
toJSONViaCodec = toJSONVia codec

toJSONVia :: ValueCodec a void -> a -> JSON.Value
toJSONVia = toContextVia

toContextVia :: Codec context a void -> a -> context
toContextVia = flip go
  where
    go :: a -> Codec context a void -> context
    go a = \case
      ValueCodec -> a
      NullCodec -> JSON.Null
      BoolCodec _ -> toJSON (a :: Bool)
      StringCodec _ -> toJSON (a :: Text)
      NumberCodec _ -> toJSON (a :: Scientific)
      ArrayCodec _ c -> toJSON (fmap (`go` c) a)
      ObjectCodec _ oc -> JSON.Object (go a oc)
      EqCodec value c -> go value c
      MapCodec _ g c -> go (g a) c
      EitherCodec c1 c2 -> case a of
        Left a1 -> go a1 c1
        Right a2 -> go a2 c2
      CommentCodec _ c -> go a c
      ReferenceCodec _ c -> go a c
      RequiredKeyCodec k c -> k JSON..= go a c
      OptionalKeyCodec k c -> case a of
        Nothing -> mempty
        Just b -> k JSON..= go b c
      PureCodec _ -> error "Cannot toJSON a pure object codec."
      ApCodec oc1 oc2 -> go a oc1 <> go a oc2
