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

toJSONVia :: Codec a void -> a -> JSON.Value
toJSONVia = flip go
  where
    go :: a -> Codec a void -> JSON.Value
    go a = \case
      NullCodec -> JSON.Null
      BoolCodec -> toJSON (a :: Bool)
      StringCodec -> toJSON (a :: Text)
      NumberCodec -> toJSON (a :: Scientific)
      ArrayCodec c -> toJSON (map (`go` c) a)
      ObjectCodec oc -> JSON.Object (goObject a oc)
      BimapCodec _ g c -> go (g a) c
      ExtraParserCodec _ g c -> go (g a) c
      SelectCodec c1 c2 -> case a of
        Left a1 -> go a1 c1
        Right a2 -> go a2 c2

    goObject :: a -> ObjectCodec a void -> JSON.Object
    goObject a = \case
      RequiredKeyCodec k c -> k JSON..= go a c
      OptionalKeyCodec k c -> case a of
        Nothing -> mempty
        Just b -> k JSON..= go b c
      PureObjectCodec _ -> error "Cannot toJSON a pure object codec."
      BimapObjectCodec _ g oc -> goObject (g a) oc
      ApObjectCodec oc1 oc2 -> goObject a oc1 <> goObject a oc2