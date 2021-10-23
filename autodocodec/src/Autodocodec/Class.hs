{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Autodocodec.Class where

import Autodocodec.Codec
import Data.Int
import Data.Scientific
import Data.Text (Text)
import Data.Word

class HasCodec a where
  -- | A codec for a single value
  --
  -- See the sections on helper functions for implementing this for plenty of examples.
  codec :: Codec a a

  -- | A codec for a list of values
  --
  -- This is really only useful for cases like 'Char' and 'String'
  listCodec :: Codec [a] [a]
  listCodec = ArrayCodec codec

field :: HasCodec output => Text -> ObjectCodec output output
field k = KeyCodec k codec

instance HasCodec Bool where
  codec = boolCodec

instance HasCodec Char where
  codec =
    let parseChar = \case
          [] -> Left "Expected exactly 1 character, but got none."
          [c] -> Right c
          _ -> Left "Expected exactly 1 character, but got more."
     in EitherCodec parseChar (: []) stringCodec
  listCodec = stringCodec

instance HasCodec String where
  codec = listCodec

instance HasCodec Text where
  codec = textCodec

instance HasCodec Scientific where
  codec = scientificCodec

instance (HasCodec l, HasCodec r) => HasCodec (Either l r) where
  codec =
    SelectCodec
      (object (field "Left"))
      (object (field "Right"))
