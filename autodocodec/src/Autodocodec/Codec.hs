{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autodocodec.Codec where

import Data.Aeson as JSON
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as T

data Codec input output where
  -- | Any value
  ValueCodec :: Codec JSON.Value JSON.Value
  -- | The 'null' value
  NullCodec :: Codec () ()
  BoolCodec :: Codec Bool Bool
  -- | A String value
  --
  -- This is named after the primitive type "String" in json, not after the haskell type string.
  StringCodec :: Codec Text Text
  NumberCodec :: Codec Scientific Scientific -- TODO can we do this without scientific?
  -- TODO use a vector here because that's what aeson uses.
  ArrayCodec ::
    !(Maybe Text) ->
    !(Codec input output) ->
    Codec [input] [output]
  ObjectCodec ::
    !(Maybe Text) ->
    !(ObjectCodec value value) ->
    Codec value value
  EqCodec ::
    (Show value, Eq value) =>
    !value ->
    !(Codec value value) ->
    Codec value value
  -- | To implement 'fmap', and to map a codec in both directions.
  BimapCodec ::
    !(oldOutput -> newOutput) ->
    !(newInput -> oldInput) ->
    !(Codec oldInput oldOutput) ->
    Codec newInput newOutput
  EitherCodec ::
    !(Codec input1 output1) ->
    !(Codec input2 output2) ->
    Codec (Either input1 input2) (Either output1 output2)
  -- For parsing with potential errors
  -- TODO: maybe we want to get rid of bimap and implement it in terms of this?
  -- TODO: this should get a better name.
  ExtraParserCodec ::
    !(oldOutput -> Either String newOutput) ->
    !(newInput -> oldInput) ->
    !(Codec oldInput oldOutput) ->
    Codec newInput newOutput
  CommentCodec :: Text -> Codec input output -> Codec input output

-- Jsut for debugging
showCodecABit :: Codec input output -> String
showCodecABit = ($ "") . go 0
  where
    go :: Int -> Codec input output -> ShowS
    go d = \case
      ValueCodec -> showString "ValueCodec"
      NullCodec -> showString "NullCodec"
      BoolCodec -> showString "BoolCodec"
      StringCodec -> showString "StringCodec"
      NumberCodec -> showString "NumberCodec"
      ArrayCodec name c -> showParen (d > 10) $ showString "ArrayCodec " . showsPrec d name . showString " " . go 11 c
      ObjectCodec name oc -> showParen (d > 10) $ showString "ObjectCodec " . showsPrec d name . showString " " . goObject 11 oc
      EqCodec value c -> showParen (d > 10) $ showString "EqCodec " . showsPrec d value . showString " " . go 11 c
      BimapCodec _ _ c -> showParen (d > 10) $ showString "BimapCodec " . go 11 c
      EitherCodec c1 c2 -> showParen (d > 10) $ showString "EitherCodec " . go 11 c1 . showString " " . go 11 c2
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

eitherCodec :: Codec input1 output1 -> Codec input2 output2 -> Codec (Either input1 input2) (Either output1 output2)
eitherCodec = EitherCodec

maybeCodec :: Codec input output -> Codec (Maybe input) (Maybe output)
maybeCodec = bimapCodec f g . EitherCodec NullCodec
  where
    f = \case
      Left () -> Nothing
      Right r -> Just r
    g = \case
      Nothing -> Left ()
      Just r -> Right r

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

object :: Text -> ObjectCodec value value -> Codec value value
object name = ObjectCodec (Just name)

boundedIntegerCodec :: (Integral i, Bounded i) => Codec i i
boundedIntegerCodec = ExtraParserCodec go fromIntegral NumberCodec
  where
    go s = case Scientific.toBoundedInteger s of
      Nothing -> Left $ "Number too big: " <> show s
      Just i -> Right i

literalText :: Text -> Codec Text Text
literalText text = EqCodec text textCodec

literalTextValue :: a -> Text -> Codec a a
literalTextValue value text = bimapCodec (const value) (const text) (literalText text)

matchChoiceCodec ::
  forall input output newInput.
  (newInput -> Maybe input, Codec input output) ->
  (newInput -> Maybe input, Codec input output) ->
  Codec newInput output
matchChoiceCodec (f1, c1) (f2, c2) =
  BimapCodec f g $
    eitherCodec c1 c2
  where
    f = foldEither
    g :: newInput -> Either input input
    g newInput = case f1 newInput of
      Just input -> Left input
      Nothing -> case f2 newInput of
        Just input -> Right input
        Nothing -> error "no match"

shownBoundedEnumCodec ::
  forall enum.
  (Show enum, Eq enum, Enum enum, Bounded enum) =>
  Codec enum enum
shownBoundedEnumCodec =
  let ls = [minBound .. maxBound]
   in case NE.nonEmpty ls of
        Nothing -> error "0 enum values ?!"
        Just ne -> stringConstCodec (NE.map (\v -> (v, T.pack (show v))) ne)

stringConstCodec ::
  forall constant.
  Eq constant =>
  NonEmpty (constant, Text) ->
  Codec constant constant
stringConstCodec = enumCodec . NE.map (\(constant, text) -> (constant, literalTextValue constant text))

enumCodec ::
  forall enum.
  Eq enum =>
  NonEmpty (enum, Codec enum enum) ->
  Codec enum enum
enumCodec = go
  where
    go :: NonEmpty (enum, Codec enum enum) -> Codec enum enum
    go ((e, c) :| rest) = case NE.nonEmpty rest of
      Nothing -> c
      Just ne ->
        matchChoiceCodec
          (\e' -> if e == e' then Just e else Nothing, c)
          (Just, go ne)

foldEither :: Either a a -> a
foldEither = \case
  Left a -> a
  Right a -> a
