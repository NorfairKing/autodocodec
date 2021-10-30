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
data Codec context input output where
  -- | Encode a 'JSON.Value', and decode any 'JSON.Value'.
  ValueCodec ::
    -- |
    ValueCodec JSON.Value JSON.Value
  -- | Encode '()' to the @null@ value, and decode @null@ as '()'.
  NullCodec ::
    -- |
    ValueCodec () ()
  -- | Encode a 'Bool' to a @boolean@ value, and decode a @boolean@ value as a 'Bool'.
  BoolCodec ::
    -- | Name of the @bool@, for error messages and documentation.
    !(Maybe Text) ->
    -- |
    ValueCodec Bool Bool
  -- | Encode 'Text' to a @string@ value, and decode a @string@ value as a 'Text'.
  --
  -- This is named after the primitive type "String" in json, not after the haskell type string.
  StringCodec ::
    -- | Name of the @string@, for error messages and documentation.
    !(Maybe Text) ->
    -- |
    ValueCodec Text Text
  -- | Encode 'Scientific' to a @number@ value, and decode a @number@ value as a 'Scientific'.
  --
  -- NOTE: We use 'Scientific' here because that is what aeson uses.
  -- TODO: Can we do this without 'Scientific'? It has too many footguns.
  NumberCodec ::
    -- | Name of the @number@, for error messages and documentation.
    !(Maybe Text) ->
    -- |
    ValueCodec Scientific Scientific
  -- | Encode a 'Vector' of values as an @array@ value, and decode an @array@ value as a 'Vector' of values.
  ArrayCodec ::
    -- | Name of the @array@, for error messages and documentation.
    !(Maybe Text) ->
    -- |
    !(ValueCodec input output) ->
    -- |
    ValueCodec (Vector input) (Vector output)
  -- | Encode a value as a an @object@ value using the given 'ObjectCodec', and decode an @object@ value as a value using the given 'ObjectCodec'.
  ObjectCodec ::
    -- | Name of the @object@, for error messages and documentation.
    !(Maybe Text) ->
    -- |
    !(ObjectCodec input output) ->
    -- |
    ValueCodec input output
  EqCodec ::
    (Show value, Eq value) =>
    -- |
    !value ->
    -- |
    !(ValueCodec value value) ->
    -- |
    ValueCodec value value
  -- | Map a codec in both directions.
  --
  -- This is not strictly dimap, because the decoding function is allowed to fail,
  -- but we can implement dimap using this function by using a decoding function that does not fail.
  -- Otherwise we would have to have another constructor here.
  MapCodec ::
    -- |
    !(oldOutput -> Either String newOutput) ->
    -- |
    !(newInput -> oldInput) ->
    -- |
    !(Codec context oldInput oldOutput) ->
    Codec context newInput newOutput
  -- | Encode/Decode an 'Either' value
  --
  -- During encoding, encode either value of an 'Either' using their own codec.
  -- During decoding, try to parse the 'Left' side first, and the 'Right' side only when that fails.
  --
  --
  -- This codec is used to implement choice.
  EitherCodec ::
    -- | Codec for the 'Left' side
    !(ValueCodec input1 output1) ->
    -- | Codec for the 'Right' side
    !(ValueCodec input2 output2) ->
    -- |
    ValueCodec (Either input1 input2) (Either output1 output2)
  -- | A comment codec
  --
  -- This is used to add implementation-irrelevant but human-relevant information.
  CommentCodec ::
    -- | Comment
    Text ->
    -- |
    ValueCodec input output ->
    -- |
    ValueCodec input output
  -- | A reference codec
  --
  -- This is used for naming a codec, so that recursive codecs can have a finite schema.
  ReferenceCodec ::
    -- |
    Text ->
    -- |
    ValueCodec input output ->
    -- |
    ValueCodec input output
  RequiredKeyCodec ::
    -- |
    Text ->
    -- |
    ValueCodec input output ->
    -- |
    ObjectCodec input output
  OptionalKeyCodec ::
    -- |
    Text ->
    -- |
    ValueCodec input output ->
    -- |
    Codec JSON.Object (Maybe input) (Maybe output)
  -- Pure is not available for non-object codecs
  PureCodec ::
    -- |
    output ->
    -- |
    ObjectCodec input output
  -- Ap is not available for non-object codecs
  ApCodec ::
    -- |
    ObjectCodec input (output -> newOutput) ->
    -- |
    ObjectCodec input output ->
    -- |
    ObjectCodec input newOutput

-- | A codec within the 'JSON.Value' context.
--
-- An 'ValueCodec' can be used to turn a Haskell value into a 'JSON.Object' or to parse a 'JSON.Object' into a haskell value.
--
-- This cannot be used in certain places where 'ObjectCodec' could be used, and vice versa.
type ValueCodec = Codec JSON.Value

-- | A codec within the 'JSON.Object' context.
--
-- An 'Object' can be used to turn a Haskell value into a 'JSON.Object' or to parse a 'JSON.Object' into a haskell value.
--
-- This cannot be used in certain places where 'ValueCodec' could be used, and vice versa.
type ObjectCodec = Codec JSON.Object

-- | A completed autodocodec
type JSONCodec a = ValueCodec a a

-- | Show a codec to a human.
--
-- This function exists for codec debugging.
-- It omits any unshowable information from the output.
showCodecABit :: Codec context input output -> String
showCodecABit = ($ "") . (`evalState` S.empty) . go 0
  where
    go :: Int -> Codec context input output -> State (Set Text) ShowS
    go d = \case
      ValueCodec -> pure $ showString "ValueCodec"
      NullCodec -> pure $ showString "NullCodec"
      BoolCodec mname -> pure $ showParen (d > 10) $ showString "BoolCodec " . showsPrec d mname
      StringCodec mname -> pure $ showParen (d > 10) $ showString "StringCodec " . showsPrec d mname
      NumberCodec mname -> pure $ showParen (d > 10) $ showString "NumberCodec " . showsPrec d mname
      ArrayCodec mname c -> (\s -> showParen (d > 10) $ showString "ArrayCodec " . showsPrec d mname . showString " " . s) <$> go 11 c
      ObjectCodec mname oc -> (\s -> showParen (d > 10) $ showString "ObjectCodec " . showsPrec d mname . showString " " . s) <$> go 11 oc
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
      RequiredKeyCodec k c -> (\s -> showParen (d > 10) $ showString "RequiredKeyCodec " . showsPrec d k . showString " " . s) <$> go 11 c
      OptionalKeyCodec k c -> (\s -> showParen (d > 10) $ showString "OptionalKeyCodec " . showsPrec d k . showString " " . s) <$> go 11 c
      PureCodec _ -> pure $ showString "PureCodec" -- TODO add show instance?
      ApCodec oc1 oc2 -> (\s1 s2 -> showParen (d > 10) $ showString "ApCodec " . s1 . showString " " . s2) <$> go 11 oc1 <*> go 11 oc2

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
  Codec context input oldOutput ->
  Codec context input newOutput
fmapCodec f = MapCodec (Right . f) id

instance Functor (Codec context input) where
  fmap = fmapCodec

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
  Codec context oldInput output ->
  Codec context newInput output
comapCodec g = MapCodec Right g

-- | Map both directions of a codec
--
-- You can use this function to change the type of a codec.
dimapCodec ::
  (oldOutput -> newOutput) ->
  (newInput -> oldInput) ->
  Codec context oldInput oldOutput ->
  Codec context newInput newOutput
dimapCodec f g = MapCodec (Right . f) g

-- | Forward-compatible version of 'PureCodec'
--
-- > pureCodec = PureCodec
pureCodec :: output -> ObjectCodec input output
pureCodec = PureCodec

-- | Forward-compatible version of 'ApCodec'
--
-- > apCodec = ApCodec
apCodec :: ObjectCodec input (output -> newOutput) -> ObjectCodec input output -> ObjectCodec input newOutput
apCodec = ApCodec

instance Applicative (ObjectCodec input) where
  pure = pureCodec
  (<*>) = apCodec

-- | Forward-compatible version of 'EitherCodec'
--
-- > eitherCodec = EitherCodec
eitherCodec ::
  ValueCodec input1 output1 ->
  ValueCodec input2 output2 ->
  ValueCodec (Either input1 input2) (Either output1 output2)
eitherCodec = EitherCodec

-- | Value or null.
maybeCodec :: ValueCodec input output -> ValueCodec (Maybe input) (Maybe output)
maybeCodec = dimapCodec f g . EitherCodec nullCodec
  where
    f = \case
      Left () -> Nothing
      Right r -> Just r
    g = \case
      Nothing -> Left ()
      Just r -> Right r

(.=) :: ObjectCodec oldInput output -> (newInput -> oldInput) -> ObjectCodec newInput output
(.=) = flip comapCodec

-- | A required field
--
-- During decoding, the field must be in the object.
--
-- During encoding, the field will always be in the object.
--
--
-- This is a forward-compatible version of 'RequiredField'
--
-- > requiredFieldWith = RequiredKeyCodec
requiredFieldWith ::
  -- | The key
  Text ->
  -- | The codec for the value
  ValueCodec input output ->
  ObjectCodec input output
requiredFieldWith = RequiredKeyCodec

-- | An optional field
--
-- During decoding, the field may be in the object. 'Nothing' will be parsed otherwise.
--
-- During encoding, the field will be in the object if it is not 'Nothing', and omitted otherwise.
--
-- This is a forward-compatible version of 'OptionalField'
--
-- > optionalFieldWith = OptionalKeyCodec
optionalFieldWith ::
  -- | The key
  Text ->
  -- | The codec for the value
  ValueCodec input output ->
  ObjectCodec (Maybe input) (Maybe output)
optionalFieldWith = OptionalKeyCodec

-- | Add a default value to a codec
--
-- During encoding, the default value is not used.
-- During decoding, the default value will be parsed if the underlying codec decodes 'Nothing'.
withDefault ::
  forall value context.
  -- | default value
  value ->
  -- |
  Codec context (Maybe value) (Maybe value) ->
  -- |
  Codec context value value
withDefault defaultValue = dimapCodec f g
  where
    f :: Maybe value -> value
    f = \case
      Nothing -> defaultValue
      Just value -> value
    g :: value -> Maybe value
    g = Just

-- | Infix version of 'withDefault'
--
-- Example usage:
--
-- > data Example = Example
-- >   { exampleText :: !Text,
-- >     exampleOptionalWithDefault :: !Text
-- >   }
-- >
-- > instance HasCodec Example where
-- >   codec =
-- >     object "Example" $
-- >       Example
-- >         <$> requiredField "text" .= exampleText
-- >         <*> optionalField "optional-with-default" .!= "default-value" .= exampleOptionalWithDefault
(.!=) ::
  forall value.
  -- |
  ObjectCodec (Maybe value) (Maybe value) ->
  -- | default value
  value ->
  -- |
  ObjectCodec value value
(.!=) = flip withDefault

-- | Infix version of 'CommentCodec'
(<?>) ::
  ValueCodec input output ->
  -- | The comment
  Text ->
  ValueCodec input output
(<?>) = flip CommentCodec

-- | A version of '<?>' that lets you supply a list of lines of text instead of a single text.
--
-- This helps when you use an automated formatter that deals with lists more nicely than with multi-line strings.
(<??>) ::
  ValueCodec input output ->
  -- | The lines of comments
  [Text] ->
  ValueCodec input output
(<??>) c ls = CommentCodec (T.unlines ls) c

-- | Forward-compatible version of 'NullCodec'
--
-- > nullCodec = NullCodec
nullCodec :: JSONCodec ()
nullCodec = NullCodec

-- | Forward-compatible version of 'BoolCodec' without a name
--
-- > boolCodec = BoolCodec Nothing
boolCodec :: JSONCodec Bool
boolCodec = BoolCodec Nothing

-- | Forward-compatible version of 'StringCodec' without a name
--
-- > textCodec = StringCodec Nothing
textCodec :: JSONCodec Text
textCodec = StringCodec Nothing

-- | A 'String' version of 'textCodec'.
--
-- WARNING: this codec uses 'T.unpack' and 'T.pack' to dimap a 'textCodec', so it DOES NOT ROUNDTRIP.
stringCodec :: JSONCodec String
stringCodec = dimapCodec T.unpack T.pack textCodec

-- | Forward-compatible version of 'NumberCodec' without a name
--
-- > scientificCodec = NumberCodec Nothing
scientificCodec :: ValueCodec Scientific Scientific
scientificCodec = NumberCodec Nothing

-- | An object codec with a given name
object :: Text -> ObjectCodec input output -> ValueCodec input output
object name = ObjectCodec (Just name)

-- | A codec for bounded integers like 'Int', 'Int8', and 'Word'.
--
-- WARNING: This codec will fail to parse numbers that are too big, for security reasons.
boundedIntegerCodec :: (Integral i, Bounded i) => JSONCodec i
boundedIntegerCodec = MapCodec go fromIntegral $ NumberCodec Nothing
  where
    go s = case Scientific.toBoundedInteger s of
      Nothing -> Left $ "Number too big: " <> show s
      Just i -> Right i

-- | A codec for a literal piece of 'Text'.
literalText :: Text -> JSONCodec Text
literalText text = EqCodec text textCodec

-- | A codec for a literal value corresponding to a literal piece of 'Text'.
literalTextValue :: a -> Text -> JSONCodec a
literalTextValue value text = dimapCodec (const value) (const text) (literalText text)

matchChoiceCodec ::
  forall input output newInput.
  -- |
  (newInput -> Maybe input, ValueCodec input output) ->
  -- |
  (newInput -> Maybe input, ValueCodec input output) ->
  -- |
  ValueCodec newInput output
matchChoiceCodec (f1, c1) (f2, c2) =
  dimapCodec f g $
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
  -- |
  NonEmpty (input -> Maybe input, ValueCodec input output) ->
  -- |
  ValueCodec input output
matchChoicesCodec ((f, c) :| rest) = case NE.nonEmpty rest of
  Nothing -> c
  Just ne ->
    matchChoiceCodec
      (f, c)
      (Just, matchChoicesCodec ne)

enumCodec ::
  forall enum.
  Eq enum =>
  -- |
  NonEmpty (enum, JSONCodec enum) ->
  -- |
  JSONCodec enum
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
  -- |
  NonEmpty (constant, Text) ->
  -- |
  JSONCodec constant
stringConstCodec =
  enumCodec
    . NE.map
      ( \(constant, text) ->
          ( constant,
            literalTextValue constant text
          )
      )

-- | A codec for a 'Bounded' 'Enum' that uses its 'Show' instance to have the values correspond to literal 'Text' values.
shownBoundedEnumCodec ::
  forall enum.
  (Show enum, Eq enum, Enum enum, Bounded enum) =>
  -- |
  JSONCodec enum
shownBoundedEnumCodec =
  let ls = [minBound .. maxBound]
   in case NE.nonEmpty ls of
        Nothing -> error "0 enum values ?!"
        Just ne -> stringConstCodec (NE.map (\v -> (v, T.pack (show v))) ne)
