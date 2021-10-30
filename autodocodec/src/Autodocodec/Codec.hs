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
import Data.Vector (Vector)

-- | A Self-documenting encoder and decoder,
--
-- also called an "Autodocodec".
--
-- In an ideal situation, this type would have only one type paramater: 'Codec value'.
-- This does not work very well because we want to be able to implement 'Functor' and 'Applicative', which each require a kind '* -> *'.
-- So instead we use two type parameters.
--
-- The two type parameters correspond to the phase in which they are used:
--
-- * The @input@ parameter is used for the type that is used during encoding of a value.
-- * The @output@ parameter is used for the type that is used during decoding of a value.
-- * Both parameters are usused during documentation.
data Codec input output where
  -- | Encode a 'JSON.Value', and decode any 'JSON.Value'.
  ValueCodec ::
    -- |
    Codec JSON.Value JSON.Value
  -- | Encode '()' to the @null@ value, and decode @null@ as '()'.
  NullCodec ::
    -- |
    Codec () ()
  -- | Encode a 'Bool' to a @boolean@ value, and decode a @boolean@ value as a 'Bool'.
  BoolCodec ::
    -- | Name of the @bool@, for error messages and documentation.
    !(Maybe Text) ->
    -- |
    Codec Bool Bool
  -- | Encode 'Text' to a @string@ value, and decode a @string@ value as a 'Text'.
  --
  -- This is named after the primitive type "String" in json, not after the haskell type string.
  StringCodec ::
    -- | Name of the @string@, for error messages and documentation.
    !(Maybe Text) ->
    -- |
    Codec Text Text
  -- | Encode 'Scientific' to a @number@ value, and decode a @number@ value as a 'Scientific'.
  --
  -- NOTE: We use 'Scientific' here because that is what aeson uses.
  -- TODO: Can we do this without 'Scientific'? It has too many footguns.
  NumberCodec ::
    -- | Name of the @number@, for error messages and documentation.
    !(Maybe Text) ->
    -- |
    Codec Scientific Scientific
  -- | Encode a 'Vector' of values as an @array@ value, and decode an @array@ value as a 'Vector' of values.
  ArrayCodec ::
    -- | Name of the @array@, for error messages and documentation.
    !(Maybe Text) ->
    -- |
    !(Codec input output) ->
    -- |
    Codec (Vector input) (Vector output)
  -- | Encode a value as a an @object@ value using the given 'ObjectCodec', and decode an @object@ value as a value using the given 'ObjectCodec'.
  ObjectCodec ::
    -- | Name of the @object@, for error messages and documentation.
    !(Maybe Text) ->
    -- |
    !(ObjectCodec value value) ->
    -- |
    Codec value value
  EqCodec ::
    (Show value, Eq value) =>
    -- |
    !value ->
    -- |
    !(Codec value value) ->
    -- |
    Codec value value
  -- | To implement 'fmap', and to map a codec in both directions.
  --
  -- This is not strictly bimap, because the decoding function is allowed to fail,
  -- but we can implement bimap using this function by using a decoding function that does not fail.
  -- Otherwise we would have to have another constructor here.
  MapCodec ::
    -- |
    !(oldOutput -> Either String newOutput) ->
    -- |
    !(newInput -> oldInput) ->
    -- |
    !(Codec oldInput oldOutput) ->
    Codec newInput newOutput
  -- | Encode/Decode an 'Either' value
  --
  -- During encoding, encode either value of an 'Either' using their own codec.
  -- During decoding, try to parse the 'Left' side first, and the 'Right' side only when that fails.
  --
  --
  -- This codec is used to implement choice.
  EitherCodec ::
    -- | Codec for the 'Left' side
    !(Codec input1 output1) ->
    -- | Codec for the 'Right' side
    !(Codec input2 output2) ->
    -- |
    Codec (Either input1 input2) (Either output1 output2)
  -- | A comment codec
  --
  -- This is used to add implementation-irrelevant but human-relevant information.
  CommentCodec ::
    -- | Comment
    Text ->
    -- |
    Codec input output ->
    -- |
    Codec input output
  -- | A reference codec
  --
  -- This is used for naming a codec, so that recursive codecs can have a finite schema.
  ReferenceCodec ::
    -- |
    Text ->
    -- |
    Codec input output ->
    -- |
    Codec input output

-- | Show a codec to a human.
--
-- This function exists for codec debugging.
-- It omits any unshowable information from the output.
showCodecABit :: Codec input output -> String
showCodecABit = ($ "") . (`evalState` S.empty) . go 0
  where
    go :: Int -> Codec input output -> State (Set Text) ShowS
    go d = \case
      ValueCodec -> pure $ showString "ValueCodec"
      NullCodec -> pure $ showString "NullCodec"
      BoolCodec mname -> pure $ showParen (d > 10) $ showString "BoolCodec " . showsPrec d mname
      StringCodec mname -> pure $ showParen (d > 10) $ showString "StringCodec " . showsPrec d mname
      NumberCodec mname -> pure $ showParen (d > 10) $ showString "NumberCodec " . showsPrec d mname
      ArrayCodec mname c -> (\s -> showParen (d > 10) $ showString "ArrayCodec " . showsPrec d mname . showString " " . s) <$> go 11 c
      ObjectCodec mname oc -> (\s -> showParen (d > 10) $ showString "ObjectCodec " . showsPrec d mname . showString " " . s) <$> goObject 11 oc
      EqCodec value c -> (\s -> showParen (d > 10) $ showString "EqCodec " . showsPrec d value . showString " " . s) <$> go 11 c
      MapCodec _ _ c -> (\s -> showParen (d > 10) $ showString "MapCodec " . s) <$> go 11 c
      EitherCodec c1 c2 -> (\s1 s2 -> showParen (d > 10) $ showString "EitherCodec " . s1 . showString " " . s2) <$> go 11 c1 <*> go 11 c2
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

-- | Map the output part of a codec
--
-- You can use this function if you only need to map the parsing-side of a codec.
-- This function is probably only useful if the function you map does not change the codec type.
--
-- WARNING: This can be used to produce a codec that does not roundtrip.
--
-- TODO example
fmapCodec ::
  (oldOutput -> newOutput) ->
  Codec input oldOutput ->
  Codec input newOutput
fmapCodec f = MapCodec (Right . f) id

-- | Map the input part of a codec
--
-- You can use this function if you only need to map the rendering-side of a codec.
-- This function is probably only useful if the function you map does not change the codec type.
--
-- WARNING: This can be used to produce a codec that does not roundtrip.
--
-- TODO example
comapCodec ::
  (newInput -> oldInput) ->
  Codec oldInput output ->
  Codec newInput output
comapCodec g = MapCodec Right g

-- | Map both directions of a codec
--
-- You can use this function to change the type of a codec.
bimapCodec ::
  (oldOutput -> newOutput) ->
  (newInput -> oldInput) ->
  Codec oldInput oldOutput ->
  Codec newInput newOutput
bimapCodec f g = MapCodec (Right . f) g

eitherCodec ::
  Codec input1 output1 ->
  Codec input2 output2 ->
  Codec (Either input1 input2) (Either output1 output2)
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

-- | An autodocodec for objects.
-- See 'Codec' for more information about the two type parameters.
data ObjectCodec input output where
  RequiredKeyCodec ::
    -- |
    Text ->
    -- |
    Codec input output ->
    -- |
    ObjectCodec input output
  OptionalKeyCodec ::
    -- |
    Text ->
    -- |
    Codec input output ->
    -- |
    ObjectCodec (Maybe input) (Maybe output)
  PureObjectCodec ::
    -- |
    output ->
    -- |
    ObjectCodec input output
  BimapObjectCodec ::
    -- |
    (oldOutput -> newOutput) ->
    -- |
    (newInput -> oldInput) ->
    -- |
    ObjectCodec oldInput oldOutput ->
    -- |
    ObjectCodec newInput newOutput
  ApObjectCodec ::
    -- |
    ObjectCodec input (output -> newOutput) ->
    -- |
    ObjectCodec input output ->
    -- |
    ObjectCodec input newOutput

fmapObjectCodec ::
  (oldOutput -> newOutput) ->
  ObjectCodec input oldOutput ->
  ObjectCodec input newOutput
fmapObjectCodec f = BimapObjectCodec f id

comapObjectCodec ::
  (newInput -> oldInput) ->
  ObjectCodec oldInput output ->
  ObjectCodec newInput output
comapObjectCodec g = BimapObjectCodec id g

bimapObjectCodec ::
  (oldOutput -> newOutput) ->
  (newInput -> oldInput) ->
  ObjectCodec oldInput oldOutput ->
  ObjectCodec newInput newOutput
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
boolCodec = BoolCodec Nothing

textCodec :: Codec Text Text
textCodec = StringCodec Nothing

stringCodec :: Codec String String
stringCodec = bimapCodec T.unpack T.pack $ StringCodec Nothing

scientificCodec :: Codec Scientific Scientific
scientificCodec = NumberCodec Nothing

object :: Text -> ObjectCodec value value -> Codec value value
object name = ObjectCodec (Just name)

boundedIntegerCodec :: (Integral i, Bounded i) => Codec i i
boundedIntegerCodec = MapCodec go fromIntegral $ NumberCodec Nothing
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
  bimapCodec f g $
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
