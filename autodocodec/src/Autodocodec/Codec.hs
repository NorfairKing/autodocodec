{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autodocodec.Codec where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Scientific as Scientific
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
  -- TODO: this should get a better name.
  ExtraParserCodec ::
    (oldOutput -> Either String newOutput) ->
    (newInput -> oldInput) ->
    Codec oldInput oldOutput ->
    Codec newInput newOutput
  CommentCodec :: Text -> Codec input output -> Codec input output

-- Jsut for debugging
showCodecABit :: Codec input output -> String
showCodecABit = ($ "") . go 0
  where
    go :: Int -> Codec input output -> ShowS
    go d = \case
      NullCodec -> showString "NullCodec"
      BoolCodec -> showString "BoolCodec"
      StringCodec -> showString "StringCodec"
      NumberCodec -> showString "NumberCodec"
      ArrayCodec c -> showParen (d > 10) $ showString "ArrayCodec " . go 11 c
      ObjectCodec oc -> showParen (d > 10) $ showString "ObjectCodec " . goObject 11 oc
      BimapCodec _ _ c -> showParen (d > 10) $ showString "BimapCodec " . go 11 c
      SelectCodec c1 c2 -> showParen (d > 10) $ showString "SelectCodec " . go 11 c1 . showString " " . go 11 c2
      ExtraParserCodec _ _ c -> showParen (d > 10) $ showString "ExtraParserCodec " . go 11 c
      CommentCodec comment c -> showParen (d > 10) $ showString "CommentCodec " . showsPrec d comment . showString " " . go 11 c
    goObject :: Int -> ObjectCodec input output -> ShowS
    goObject d = \case
      RequiredKeyCodec k c -> showParen (d > 10) $ showString "RequiredKeyCodec " . showsPrec d k . showString " " . go 11 c
      OptionalKeyCodec k c -> showParen (d > 10) $ showString "OptionalKeyCodec " . showsPrec d k . showString " " . go 11 c
      PureObjectCodec _ -> showString "PureObjectCodec" -- TODO add show instance?
      BimapObjectCodec _ _ oc -> showParen (d > 10) $ showString "BimapObjectCodec " . goObject 11 oc
      ApObjectCodec oc1 oc2 -> showParen (d > 10) $ showString "KeyCodec " . goObject 11 oc1 . showString " " . goObject 11 oc2

fmapCodec :: (oldOutput -> newOutput) -> Codec input oldOutput -> Codec input newOutput
fmapCodec f = BimapCodec f id

comapCodec :: (newInput -> oldInput) -> Codec oldInput output -> Codec newInput output
comapCodec g = BimapCodec id g

bimapCodec :: (oldOutput -> newOutput) -> (newInput -> oldInput) -> Codec oldInput oldOutput -> Codec newInput newOutput
bimapCodec = BimapCodec

selectCodec ::
  Codec input1 output1 -> Codec input2 output2 -> Codec (Either input1 input2) (Either output1 output2)
selectCodec = SelectCodec

eitherCodec :: Codec input1 output1 -> Codec input2 output2 -> Codec (Either input1 input2) (Either output1 output2)
eitherCodec = SelectCodec

maybeCodec :: Codec input output -> Codec (Maybe input) (Maybe output)
maybeCodec = bimapCodec f g . SelectCodec NullCodec
  where
    f = \case
      Left () -> Nothing
      Right r -> Just r
    g = \case
      Nothing -> Left ()
      Just r -> Right r

choiceCodec :: NonEmpty (Codec input output) -> Codec input output
choiceCodec (c1 :| rest) = case NE.nonEmpty rest of
  Nothing -> c1
  Just ne ->
    let f = \case
          Left a -> a
          Right a -> a
        g = Right
     in bimapCodec f g (SelectCodec c1 (choiceCodec ne))

instance Functor (Codec input) where
  fmap = fmapCodec

data ObjectCodec input output where
  RequiredKeyCodec :: Text -> Codec input output -> ObjectCodec input output
  OptionalKeyCodec :: Text -> Codec input output -> ObjectCodec (Maybe input) (Maybe output)
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

(<?>) :: Codec input output -> Text -> Codec input output
(<?>) = flip CommentCodec

(<??>) :: Codec input output -> [Text] -> Codec input output
(<??>) c ls = CommentCodec (T.unlines ls) c

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

boundedIntegerCodec :: (Integral i, Bounded i) => Codec i i
boundedIntegerCodec = ExtraParserCodec go fromIntegral NumberCodec
  where
    go s = case Scientific.toBoundedInteger s of
      Nothing -> Left $ "Number too big: " <> show s
      Just i -> Right i
