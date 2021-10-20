{-# LANGUAGE FlexibleInstances #-}

module Autodocodec.Class where

import Autodocodec.Codec
import Data.Text (Text)

class HasCodec a where
  codec :: Codec a a

field :: HasCodec output => Text -> ObjectCodec output output
field k = KeyCodec k codec
