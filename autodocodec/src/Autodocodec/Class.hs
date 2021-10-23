{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Autodocodec.Class where

import Autodocodec.Codec
import Data.Scientific
import Data.Text (Text)

class HasCodec a where
  codec :: Codec a a

field :: HasCodec output => Text -> ObjectCodec output output
field k = KeyCodec k codec

instance HasCodec Bool where
  codec = boolCodec

instance HasCodec Text where
  codec = textCodec

instance HasCodec Scientific where
  codec = scientificCodec

instance (HasCodec l, HasCodec r) => HasCodec (Either l r) where
  codec =
    SelectCodec
      (object (field "Left"))
      (object (field "Right"))
