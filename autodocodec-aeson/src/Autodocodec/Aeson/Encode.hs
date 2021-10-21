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
      ChoiceCodec _ c -> go a (c a)
    -- ApCodec _ _ -> error "Cannot toJSON Ap with non-object codecs."
    -- PureCodec _ -> error "Cannot toJSON a pure codec."
    -- AltCodecs _ -> error "Cannot toJSON an Alt codec."

    goObject :: a -> ObjectCodec a void -> JSON.Object
    goObject a = \case
      KeyCodec k c -> k JSON..= go a c
      -- EqObjectCodec o oc -> goObject o oc
      PureObjectCodec _ -> error "Cannot toJSON a pure object codec."
      BimapObjectCodec _ g oc -> goObject (g a) oc
      ApObjectCodec oc1 oc2 -> goObject a oc1 <> goObject a oc2
