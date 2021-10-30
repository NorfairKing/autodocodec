{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autodocodec.Codec where

import Control.Monad.State
import Data.Aeson as JSON
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Scientific as Scientific
import Data.Set (Set)
import qualified Data.Set as S
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
  -- For adding implementation-irrelevant but human-relevant information.
  CommentCodec :: Text -> Codec input output -> Codec input output
  -- For naming a codec, so that recursive codecs can have a finite schema.
  ReferenceCodec :: Text -> Codec input output -> Codec input output

-- Jsut for debugging
showCodecABit :: Codec input output -> String
showCodecABit = ($ "") . (`evalState` S.empty) . go 0
  where
    go :: Int -> Codec input output -> State (Set Text) ShowS
    go d = \case
      ValueCodec -> pure $ showString "ValueCodec"
      NullCodec -> pure $ showString "NullCodec"
      BoolCodec -> pure $ showString "BoolCodec"
      StringCodec -> pure $ showString "StringCodec"
      NumberCodec -> pure $ showString "NumberCodec"
      ArrayCodec name c -> (\s -> showParen (d > 10) $ showString "ArrayCodec " . showsPrec d name . showString " " . s) <$> go 11 c
      ObjectCodec name oc -> (\s -> showParen (d > 10) $ showString "ObjectCodec " . showsPrec d name . showString " " . s) <$> goObject 11 oc
      EqCodec value c -> (\s -> showParen (d > 10) $ showString "EqCodec " . showsPrec d value . showString " " . s) <$> go 11 c
      BimapCodec _ _ c -> (\s -> showParen (d > 10) $ showString "BimapCodec " . s) <$> go 11 c
      EitherCodec c1 c2 -> (\s1 s2 -> showParen (d > 10) $ showString "EitherCodec " . s1 . showString " " . s2) <$> go 11 c1 <*> go 11 c2
      ExtraParserCodec _ _ c -> (\s -> showParen (d > 10) $ showString "ExtraParserCodec " . s) <$> go 11 c
      CommentCodec comment c -> (\s -> showParen (d > 10) $ showString "CommentCodec " . showsPrec d comment . showString " " . s) <$> go 11 c
      ReferenceCodec name c -> do
        alreadySeen <- gets (S.member name)
        if alreadySeen
          then pure $ showParen (d > 10) $ showString "ReferenceCodec " . showsPrec d name
          else do
            modify (S.insert name)
            s <- go d c
            pure $ showParen (d > 10) $ showString "ReferenceCodec " . showsPrec d name . showString " " . s

    goObject :: Int -> ObjectCodec input output -> State (Set Text) ShowS
    goObject d = \case
      RequiredKeyCodec k c -> (\s -> showParen (d > 10) $ showString "RequiredKeyCodec " . showsPrec d k . showString " " . s) <$> go 11 c
      OptionalKeyCodec k c -> (\s -> showParen (d > 10) $ showString "OptionalKeyCodec " . showsPrec d k . showString " " . s) <$> go 11 c
      PureObjectCodec _ -> pure $ showString "PureObjectCodec" -- TODO add show instance?
      BimapObjectCodec _ _ oc -> (\s -> showParen (d > 10) $ showString "BimapObjectCodec " . s) <$> goObject 11 oc
      ApObjectCodec oc1 oc2 -> (\s1 s2 -> showParen (d > 10) $ showString "KeyCodec " . s1 . showString " " . s2) <$> goObject 11 oc1 <*> goObject 11 oc2

fmapCodec :: (oldOutput -> newOutput) -> Codec input oldOutput -> Codec input newOutput
fmapCodec f = BimapCodec f id

comapCodec :: (newInput -> oldInput) -> Codec oldInput output -> Codec newInput output
comapCodec g = BimapCodec id g

bimapCodec :: (oldOutput -> newOutput) -> (newInput -> oldInput) -> Codec oldInput oldOutput -> Codec newInput newOutput
bimapCodec = BimapCodec

eitherCodec :: Codec input1 output1 -> Codec input2 output2 -> Codec (Either input1 input2) (Either output1 output2)
eitherCodec = EitherCodec

-- | Value or null.
maybeCodec :: Codec input output -> Codec (Maybe input) (Maybe output)
maybeCodec = bimapCodec f g . EitherCodec nullCodec
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

nullCodec :: Codec () ()
nullCodec = NullCodec

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
    f = \case
      Left a -> a
      Right a -> a
    g :: newInput -> Either input input
    g newInput = case f1 newInput of
      Just input -> Left input
      Nothing -> case f2 newInput of
        Just input -> Right input
        Nothing -> error "no match"

matchChoicesCodec ::
  forall input output.
  NonEmpty (input -> Maybe input, Codec input output) ->
  Codec input output
matchChoicesCodec ((f, c) :| rest) = case NE.nonEmpty rest of
  Nothing -> c
  Just ne ->
    matchChoiceCodec
      (f, c)
      (Just, matchChoicesCodec ne)

enumCodec ::
  forall enum.
  Eq enum =>
  NonEmpty (enum, Codec enum enum) ->
  Codec enum enum
enumCodec =
  matchChoicesCodec
    . NE.map
      ( \(constant, c) ->
          ( \constant' ->
              if constant' == constant
                then Just constant'
                else Nothing,
            c
          )
      )

stringConstCodec ::
  forall constant.
  Eq constant =>
  NonEmpty (constant, Text) ->
  Codec constant constant
stringConstCodec =
  enumCodec
    . NE.map
      ( \(constant, text) ->
          ( constant,
            literalTextValue constant text
          )
      )

shownBoundedEnumCodec ::
  forall enum.
  (Show enum, Eq enum, Enum enum, Bounded enum) =>
  Codec enum enum
shownBoundedEnumCodec =
  let ls = [minBound .. maxBound]
   in case NE.nonEmpty ls of
        Nothing -> error "0 enum values ?!"
        Just ne -> stringConstCodec (NE.map (\v -> (v, T.pack (show v))) ne)
