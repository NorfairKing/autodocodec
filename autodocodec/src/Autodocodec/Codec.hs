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

data Codec input output where
  NullCodec :: Codec () ()
  BoolCodec :: Codec Bool Bool
  StringCodec :: Codec Text Text
  NumberCodec :: Codec Scientific Scientific -- TODO can we do this without scientific?
  ObjectCodec ::
    ObjectCodec value value ->
    Codec value value
  -- | To implement 'fmap', and to map a codec in both directions.
  BimapCodec ::
    (oldOutput -> newOutput) ->
    (newInput -> oldInput) ->
    Codec oldInput oldOutput ->
    Codec newInput newOutput
  -- -- | To implement 'pure'
  -- PureCodec ::
  --   output ->
  --   Codec input output
  -- -- | To implement '<*>'
  -- --
  -- -- This really doesn't make much sense, but we need it to have an alternative instance.
  -- -- TODO maybe get rid of it altogether and just implement it with errors?
  -- ApCodec ::
  --   Codec input (oldOutput -> output) ->
  --   Codec input oldOutput ->
  --   Codec input output

  -- | To implement '<|>'
  -- AltCodecs ::
  --   [Codec input output] ->
  --   Codec input output
  -- ChoiceCodec :: [Codec oldInput output] -> (input -> Codec input oldOutput) -> Codec input output
  SelectCodec :: Codec input1 output1 -> Codec input2 output2 -> Codec (Either input1 input2) (Either output1 output2)

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

-- pureCodec :: a -> Codec void a
-- pureCodec = PureCodec
--
-- apCodec :: Codec input (oldOutput -> output) -> Codec input oldOutput -> Codec input output
-- apCodec = ApCodec
--
-- instance Applicative (Codec input) where
--   pure = pureCodec
--   (<*>) = apCodec
--
-- emptyCodec :: Codec input output
-- emptyCodec = AltCodecs []
--
-- orCodec :: Codec input output -> Codec input output -> Codec input output
-- orCodec c1 c2 = AltCodecs [c1, c2]
--
-- choice :: [Codec input output] -> Codec input output
-- choice = AltCodecs
--
-- instance Alternative (Codec input) where
--   empty = emptyCodec
--   (<|>) = orCodec

-- choice :: [Codec oldInput output] -> (input -> Codec input oldOutput) -> Codec input output
-- choice = ChoiceCodec

data ObjectCodec input output where
  KeyCodec :: Text -> Codec input output -> ObjectCodec input output
  -- EqObjectCodec :: (Show newInput, Eq newInput) => newInput -> ObjectCodec oldInput output -> ObjectCodec newInput output
  PureObjectCodec :: output -> ObjectCodec input output
  BimapObjectCodec :: (oldOutput -> newOutput) -> (newInput -> oldInput) -> ObjectCodec oldInput oldOutput -> ObjectCodec newInput newOutput
  ApObjectCodec :: ObjectCodec input (output -> newOutput) -> ObjectCodec input output -> ObjectCodec input newOutput
  SelectObjectCodec :: ObjectCodec input1 output1 -> ObjectCodec input2 output2 -> ObjectCodec (Either input1 input2) (Either output1 output2)

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

scientificCodec :: Codec Scientific Scientific
scientificCodec = NumberCodec

object :: ObjectCodec value value -> Codec value value
object = ObjectCodec
