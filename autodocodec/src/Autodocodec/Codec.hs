{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autodocodec.Codec where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T

data Codec input output where
  NullCodec :: Codec () ()
  BoolCodec :: Codec Bool Bool
  StringCodec :: Codec Text Text
  NumberCodec :: Codec Scientific Scientific -- TODO can we do this without scientific?
  -- TODO use a vector here because that's what aeson uses.
  ArrayCodec :: Codec input output -> Codec [input] [output]
  ObjectCodec ::
    ObjectCodec value value ->
    Codec value value
  -- | To implement 'fmap', and to map a codec in both directions.
  BimapCodec ::
    (oldOutput -> newOutput) ->
    (newInput -> oldInput) ->
    Codec oldInput oldOutput ->
    Codec newInput newOutput
  SelectCodec :: Codec input1 output1 -> Codec input2 output2 -> Codec (Either input1 input2) (Either output1 output2)
  -- For parsing with potential errors
  -- TODO: maybe we want to get rid of bimap and implement it in terms of this?
  EitherCodec ::
    (oldOutput -> Either String newOutput) ->
    (newInput -> oldInput) ->
    Codec oldInput oldOutput ->
    Codec newInput newOutput

choiceCodec :: NonEmpty (Codec input output) -> Codec input output
choiceCodec (c1 :| rest) = case NE.nonEmpty rest of
  Nothing -> c1
  Just ne ->
    let f = \case
          Left a -> a
          Right a -> a
        g = Right
     in bimapCodec f g (SelectCodec c1 (choiceCodec ne))

fmapCodec :: (oldOutput -> newOutput) -> Codec input oldOutput -> Codec input newOutput
fmapCodec f = BimapCodec f id

comapCodec :: (newInput -> oldInput) -> Codec oldInput output -> Codec newInput output
comapCodec g = BimapCodec id g

bimapCodec :: (oldOutput -> newOutput) -> (newInput -> oldInput) -> Codec oldInput oldOutput -> Codec newInput newOutput
bimapCodec = BimapCodec

instance Functor (Codec input) where
  fmap = fmapCodec

data ObjectCodec input output where
  KeyCodec :: Text -> Codec input output -> ObjectCodec input output
  PureObjectCodec :: output -> ObjectCodec input output
  BimapObjectCodec :: (oldOutput -> newOutput) -> (newInput -> oldInput) -> ObjectCodec oldInput oldOutput -> ObjectCodec newInput newOutput
  ApObjectCodec :: ObjectCodec input (output -> newOutput) -> ObjectCodec input output -> ObjectCodec input newOutput

fmapObjectCodec :: (oldOutput -> newOutput) -> ObjectCodec input oldOutput -> ObjectCodec input newOutput
fmapObjectCodec f = BimapObjectCodec f id

comapObjectCodec :: (newInput -> oldInput) -> ObjectCodec oldInput output -> ObjectCodec newInput output
comapObjectCodec g = BimapObjectCodec id g

bimapObjectCodec :: (oldOutput -> newOutput) -> (newInput -> oldInput) -> ObjectCodec oldInput oldOutput -> ObjectCodec newInput newOutput
bimapObjectCodec = BimapObjectCodec

instance Functor (ObjectCodec input) where
  fmap = fmapObjectCodec

instance Applicative (ObjectCodec input) where
  pure = pureObjectCodec
  (<*>) = apObjectCodec

pureObjectCodec :: output -> ObjectCodec input output
pureObjectCodec = PureObjectCodec

apObjectCodec :: ObjectCodec input (output -> newOutput) -> ObjectCodec input output -> ObjectCodec input newOutput
apObjectCodec = ApObjectCodec

(.=) :: ObjectCodec oldInput output -> (newInput -> oldInput) -> ObjectCodec newInput output
(.=) = flip comapObjectCodec

-- (.==) :: (Show newInput, Eq newInput) => ObjectCodec oldInput output -> newInput -> ObjectCodec newInput output
-- (.==) = flip EqObjectCodec

boolCodec :: Codec Bool Bool
boolCodec = BoolCodec

textCodec :: Codec Text Text
textCodec = StringCodec

stringCodec :: Codec String String
stringCodec = BimapCodec T.unpack T.pack StringCodec

scientificCodec :: Codec Scientific Scientific
scientificCodec = NumberCodec

object :: ObjectCodec value value -> Codec value value
object = ObjectCodec
