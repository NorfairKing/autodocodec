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
      NullCodec -> toJSON (a :: ())
      BoolCodec -> toJSON (a :: Bool)
      StringCodec -> toJSON (a :: Text)
      NumberCodec -> toJSON (a :: Scientific)
      ObjectCodec oc -> JSON.Object (goObject a oc)
      BimapCodec _ g c -> go (g a) c
      SelectCodec c1 c2 -> case a of
        Left a1 -> go a1 c1
        Right a2 -> go a2 c2

    goObject :: a -> ObjectCodec a void -> JSON.Object
    goObject a = \case
      KeyCodec k c -> k JSON..= go a c
      PureObjectCodec _ -> error "Cannot toJSON a pure object codec."
      BimapObjectCodec _ g oc -> goObject (g a) oc
      ApObjectCodec oc1 oc2 -> goObject a oc1 <> goObject a oc2
      SelectObjectCodec oc1 oc2 -> case a of
        Left a1 -> goObject a1 oc1
        Right a2 -> goObject a2 oc2
