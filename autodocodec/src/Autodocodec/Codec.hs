{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Autodocodec.Codec where

import Control.Monad.State
import Data.Aeson as JSON
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Scientific as Scientific
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

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
  -- | Encode '()' to the @null@ value, and decode @null@ as '()'.
  NullCodec ::
    -- |
    ValueCodec () ()
  -- | Encode a 'Bool' to a @boolean@ value, and decode a @boolean@ value as a 'Bool'.
  BoolCodec ::
    -- | Name of the @bool@, for error messages and documentation.
    (Maybe Text) ->
    -- |
    JSONCodec Bool
  -- | Encode 'Text' to a @string@ value, and decode a @string@ value as a 'Text'.
  --
  -- This is named after the primitive type "String" in json, not after the haskell type string.
  StringCodec ::
    -- | Name of the @string@, for error messages and documentation.
    (Maybe Text) ->
    -- |
    JSONCodec Text
  -- | Encode 'Scientific' to a @number@ value, and decode a @number@ value as a 'Scientific'.
  --
  -- NOTE: We use 'Scientific' here because that is what aeson uses.
  NumberCodec ::
    -- | Name of the @number@, for error messages and documentation.
    (Maybe Text) ->
    -- |
    JSONCodec Scientific
  -- | Encode a 'HashMap', and decode any 'HashMap'.
  HashMapCodec ::
    (Eq k, Hashable k, FromJSONKey k, ToJSONKey k) =>
    -- |
    JSONCodec v ->
    -- |
    JSONCodec (HashMap k v)
  -- | Encode a 'Map', and decode any 'Map'.
  MapCodec ::
    (Ord k, FromJSONKey k, ToJSONKey k) =>
    -- |
    JSONCodec v ->
    -- |
    JSONCodec (Map k v)
  -- | Encode a 'JSON.Value', and decode any 'JSON.Value'.
  ValueCodec ::
    -- |
    JSONCodec JSON.Value
  -- | Encode a 'Vector' of values as an @array@ value, and decode an @array@ value as a 'Vector' of values.
  ArrayOfCodec ::
    -- | Name of the @array@, for error messages and documentation.
    (Maybe Text) ->
    -- |
    (ValueCodec input output) ->
    -- |
    ValueCodec (Vector input) (Vector output)
  -- | Encode a value as a an @object@ value using the given 'ObjectCodec', and decode an @object@ value as a value using the given 'ObjectCodec'.
  ObjectOfCodec ::
    -- | Name of the @object@, for error messages and documentation.
    (Maybe Text) ->
    -- |
    (ObjectCodec input output) ->
    -- |
    ValueCodec input output
  -- | Match a given value using its 'Eq' instance during decoding, and encode exactly that value during encoding.
  EqCodec ::
    (Show value, Eq value) =>
    -- | Value to match
    value ->
    -- | Codec for the value
    JSONCodec value ->
    -- |
    JSONCodec value
  -- | Map a codec in both directions.
  --
  -- This is not strictly dimap, because the decoding function is allowed to fail,
  -- but we can implement dimap using this function by using a decoding function that does not fail.
  -- Otherwise we would have to have another constructor here.
  BimapCodec ::
    -- |
    (oldOutput -> Either String newOutput) ->
    -- |
    (newInput -> oldInput) ->
    -- |
    (Codec context oldInput oldOutput) ->
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
    (ValueCodec input1 output1) ->
    -- | Codec for the 'Right' side
    (ValueCodec input2 output2) ->
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
  --
  -- It doesn't _need_ to be recursive, and you may just have wanted to name the codec, but it _may_ be recursive from here downward.
  --
  -- This value MUST be lazy, otherwise we can never define recursive codecs.
  ReferenceCodec ::
    -- | Name
    Text ->
    ~(ValueCodec input output) ->
    ValueCodec input output
  RequiredKeyCodec ::
    -- | Key
    Text ->
    -- | Codec for the value
    ValueCodec input output ->
    -- | Documentation
    Maybe Text ->
    -- |
    ObjectCodec input output
  OptionalKeyCodec ::
    -- | Key
    Text ->
    -- | Codec for the value
    ValueCodec input output ->
    -- | Documentation
    Maybe Text ->
    -- |
    ObjectCodec (Maybe input) (Maybe output)
  OptionalKeyWithDefaultCodec ::
    -- | Key
    Text ->
    -- | Codec for the value
    ValueCodec value value ->
    -- | Default value
    value ->
    -- | Documentation
    Maybe Text ->
    -- |
    ObjectCodec value value
  -- | To implement 'pure' from 'Applicative'.
  --
  -- Pure is not available for non-object codecs because there is no 'mempty' for 'JSON.Value', which we would need during encoding.
  PureCodec ::
    -- |
    output ->
    -- |
    --
    -- We have to use 'void' instead of 'Void' here to be able to implement 'Applicative'.
    ObjectCodec void output
  -- | To implement '<*>' from 'Applicative'.
  --
  -- Ap is not available for non-object codecs because we cannot combine ('mappend') two encoded 'JSON.Value's
  ApCodec ::
    -- |
    ObjectCodec input (output -> newOutput) ->
    -- |
    ObjectCodec input output ->
    -- |
    ObjectCodec input newOutput

-- | A codec within the 'JSON.Value' context.
--
-- An 'ValueCodec' can be used to turn a Haskell value into a 'JSON.Value' or to parse a 'JSON.Value' into a haskell value.
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
--
-- You can use a value of this type to get everything else for free:
--
-- * Encode values to JSON using 'toJSONViaCodec' from @autodocodec-aeson@
-- * Decode values from JSON using 'parseJSONViaCodec' from @autodocodec-aeson@
-- * Produce a JSON Schema using 'jsonSchemaViaCodec' from @autodocodec-aeson@
-- * Produce a human-readible YAML schema using @renderColouredSchemaViaCodec@ from @autodocodec-yaml@
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
      NullCodec -> pure $ showString "NullCodec"
      BoolCodec mName -> pure $ showParen (d > 10) $ showString "BoolCodec " . showsPrec 11 mName
      StringCodec mName -> pure $ showParen (d > 10) $ showString "StringCodec " . showsPrec 11 mName
      NumberCodec mName -> pure $ showParen (d > 10) $ showString "NumberCodec " . showsPrec 11 mName
      ArrayOfCodec mName c -> (\s -> showParen (d > 10) $ showString "ArrayOfCodec " . showsPrec 11 mName . showString " " . s) <$> go 11 c
      ObjectOfCodec mName oc -> (\s -> showParen (d > 10) $ showString "ObjectOfCodec " . showsPrec 11 mName . showString " " . s) <$> go 11 oc
      ValueCodec -> pure $ showString "ValueCodec"
      MapCodec c -> (\s -> showParen (d > 10) $ showString "MapCodec" . s) <$> go 11 c
      HashMapCodec c -> (\s -> showParen (d > 10) $ showString "HashMapCodec" . s) <$> go 11 c
      EqCodec value c -> (\s -> showParen (d > 10) $ showString "EqCodec " . showsPrec 11 value . showString " " . s) <$> go 11 c
      BimapCodec _ _ c -> (\s -> showParen (d > 10) $ showString "BimapCodec _ _ " . s) <$> go 11 c
      EitherCodec c1 c2 -> (\s1 s2 -> showParen (d > 10) $ showString "EitherCodec " . s1 . showString " " . s2) <$> go 11 c1 <*> go 11 c2
      CommentCodec comment c -> (\s -> showParen (d > 10) $ showString "CommentCodec " . showsPrec 11 comment . showString " " . s) <$> go 11 c
      ReferenceCodec name c -> do
        alreadySeen <- gets (S.member name)
        if alreadySeen
          then pure $ showParen (d > 10) $ showString "ReferenceCodec " . showsPrec 11 name
          else do
            modify (S.insert name)
            s <- go 11 c
            pure $ showParen (d > 10) $ showString "ReferenceCodec " . showsPrec 11 name . showString " " . s
      RequiredKeyCodec k c mdoc -> (\s -> showParen (d > 10) $ showString "RequiredKeyCodec " . showsPrec 11 k . showString " " . showsPrec 11 mdoc . showString " " . s) <$> go 11 c
      OptionalKeyCodec k c mdoc -> (\s -> showParen (d > 10) $ showString "OptionalKeyCodec " . showsPrec 11 k . showString " " . showsPrec 11 mdoc . showString " " . s) <$> go 11 c
      OptionalKeyWithDefaultCodec k c _ mdoc -> (\s -> showParen (d > 10) $ showString "OptionalKeyWithDefaultCodec " . showsPrec 11 k . showString " " . s . showString " _ " . showsPrec 11 mdoc) <$> go 11 c
      PureCodec _ -> pure $ showString "PureCodec _"
      ApCodec oc1 oc2 -> (\s1 s2 -> showParen (d > 10) $ showString "ApCodec " . s1 . showString " " . s2) <$> go 11 oc1 <*> go 11 oc2

-- | Map the output part of a codec
--
-- You can use this function if you only need to map the parsing-side of a codec.
-- This function is probably only useful if the function you map does not change the codec type.
--
-- WARNING: This can be used to produce a codec that does not roundtrip.
--
-- >>> JSON.parseMaybe (parseJSONVia (rmapCodec (*2) codec)) (Number 5) :: Maybe Int
-- Just 10
rmapCodec ::
  (oldOutput -> newOutput) ->
  Codec context input oldOutput ->
  Codec context input newOutput
rmapCodec f = dimapCodec f id

instance Functor (Codec context input) where
  fmap = rmapCodec

-- | Map the input part of a codec
--
-- You can use this function if you only need to map the rendering-side of a codec.
-- This function is probably only useful if the function you map does not change the codec type.
--
-- WARNING: This can be used to produce a codec that does not roundtrip.
--
-- >>> toJSONVia (lmapCodec (*2) (codec :: JSONCodec Int)) 5
-- Number 10.0
lmapCodec ::
  (newInput -> oldInput) ->
  Codec context oldInput output ->
  Codec context newInput output
lmapCodec g = dimapCodec id g

-- | Infix version of 'lmapCodec'
--
-- Use this function to supply the rendering side of a codec.
--
-- > (.=) = flip lmapCodec
--
-- === Example usage
--
-- > data Example = Example
-- >   { exampleText :: !Text,
-- >     exampleBool :: !Bool
-- >   }
-- > instance HasCodec Example where
-- >   codec =
-- >     object "Example" $
-- >       Example
-- >         <$> requiredField "text" .= exampleText
-- >         <*> requiredField "bool" .= exampleBool
(.=) :: ObjectCodec oldInput output -> (newInput -> oldInput) -> ObjectCodec newInput output
(.=) = flip lmapCodec

-- | Map both directions of a codec
--
-- You can use this function to change the type of a codec as long as the two
-- functions are inverses.
-- A good use-case is implementing 'HasCodec' for newtypes:
--
-- > newtype MyInt = MyInt { unMyInt :: Int }
-- > instance HasCodec MyInt where
-- >   codec = dimapCodec MyInt unMyInt codec
dimapCodec ::
  -- | Function to make __to__ the new type
  (oldOutput -> newOutput) ->
  -- | Function to make __from__ the new type
  (newInput -> oldInput) ->
  -- | Codec for the old type
  Codec context oldInput oldOutput ->
  Codec context newInput newOutput
dimapCodec f g = bimapCodec (Right . f) g

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

-- | Also allow @null@ during decoding of a 'Maybe' value.
--
-- During decoding, also accept a @null@ value as 'Nothing'.
-- During encoding, encode as usual.
--
--
-- === Example usage
--
-- >>> toJSONVia (maybeCodec codec) (Just 'a')
-- String "a"
-- >>> toJSONVia (maybeCodec codec) (Nothing :: Maybe Char)
-- Null
maybeCodec :: ValueCodec input output -> ValueCodec (Maybe input) (Maybe output)
maybeCodec = dimapCodec f g . EitherCodec nullCodec
  where
    f = \case
      Left () -> Nothing
      Right r -> Just r
    g = \case
      Nothing -> Left ()
      Just r -> Right r

-- | Forward-compatible version of 'EitherCodec'
--
-- > eitherCodec = EitherCodec
--
-- === Example usage
--
-- >>> toJSONVia (eitherCodec (codec :: JSONCodec Int) (codec :: JSONCodec String)) (Left 5)
-- Number 5.0
-- >>> toJSONVia (eitherCodec (codec :: JSONCodec Int) (codec :: JSONCodec String)) (Right "hello")
-- String "hello"
eitherCodec ::
  ValueCodec input1 output1 ->
  ValueCodec input2 output2 ->
  ValueCodec (Either input1 input2) (Either output1 output2)
eitherCodec = EitherCodec

-- | Map a codec's input and output types.
--
-- This function allows you to have the parsing fail in a new way.
--
-- === Example usage
--
-- > boundedIntegerCodec :: (Integral i, Bounded i) => JSONCodec i
-- > boundedIntegerCodec = bimapCodec go fromIntegral $ NumberCodec Nothing
-- >   where
-- >     go s = case Scientific.toBoundedInteger s of
-- >       Nothing -> Left $ "Number too big: " <> show s
-- >       Just i -> Right i
bimapCodec ::
  (oldOutput -> Either String newOutput) ->
  (newInput -> oldInput) ->
  Codec context oldInput oldOutput ->
  Codec context newInput newOutput
bimapCodec = BimapCodec

-- | Forward-compatible version of 'ArrayCodec' without a name
--
-- > arrayCodec = ArrayOfCodec Nothing
--
-- === Example usage
--
-- >>> toJSONVia (listCodec codec) (Array.fromList ['a','b'])
-- Array [String "a", String "b"]
arrayCodec :: ValueCodec input output -> ValueCodec (Vector input) (Vector output)
arrayCodec = ArrayOfCodec Nothing

-- | Build a codec for lists of values from a codec for a single value.
--
-- === Example usage
--
-- >>> toJSONVia (listCodec codec) ['a','b']
-- Array [String "a",String "b"]
listCodec :: ValueCodec input output -> ValueCodec [input] [output]
listCodec = dimapCodec V.toList V.fromList . arrayCodec

-- | Build a codec for nonempty lists of values from a codec for a single value.
--
-- === Example usage
--
-- >>> toJSONVia (nonEmptyCodec codec) ('a' :| ['b'])
-- Array [String "a",String "b"]
nonEmptyCodec :: ValueCodec input output -> ValueCodec (NonEmpty input) (NonEmpty output)
nonEmptyCodec = bimapCodec parseNonEmptyList NE.toList . listCodec
  where
    parseNonEmptyList l = case NE.nonEmpty l of
      Nothing -> Left "Expected a nonempty list, but got an empty list."
      Just ne -> Right ne

-- | A required field
--
-- During decoding, the field must be in the object.
--
-- During encoding, the field will always be in the object.
requiredFieldWith ::
  -- | Key
  Text ->
  -- | Codec for the value
  ValueCodec input output ->
  -- | Documentation
  Text ->
  ObjectCodec input output
requiredFieldWith key c doc = RequiredKeyCodec key c (Just doc)

-- | Like 'requiredFieldWith', but without documentation.
requiredFieldWith' ::
  -- | Key
  Text ->
  -- | Codec for the value
  ValueCodec input output ->
  ObjectCodec input output
requiredFieldWith' key c = RequiredKeyCodec key c Nothing

-- | An optional field
--
-- During decoding, the field may be in the object. 'Nothing' will be parsed otherwise.
--
-- During encoding, the field will be in the object if it is not 'Nothing', and omitted otherwise.
optionalFieldWith ::
  -- | Key
  Text ->
  -- | Codec for the value
  ValueCodec input output ->
  -- | Documentation
  Text ->
  ObjectCodec (Maybe input) (Maybe output)
optionalFieldWith key c doc = OptionalKeyCodec key c (Just doc)

-- | Like 'optionalFieldWith', but without documentation.
optionalFieldWith' ::
  -- | Key
  Text ->
  -- | Codec for the value
  ValueCodec input output ->
  ObjectCodec (Maybe input) (Maybe output)
optionalFieldWith' key c = OptionalKeyCodec key c Nothing

-- | An optional field with default value
--
-- During decoding, the field may be in the object. The default value will be parsed otherwise.
--
-- During encoding, the field will be in the object. The default value is ignored.
--
-- The shown version of the default value will appear in the documentation.
optionalFieldWithDefaultWith ::
  -- | Key
  Text ->
  -- | Codec for the value
  ValueCodec output output ->
  -- | Default value
  output ->
  -- | Documentation
  Text ->
  ObjectCodec output output
optionalFieldWithDefaultWith key c defaultValue doc = OptionalKeyWithDefaultCodec key c defaultValue (Just doc)

-- | Like 'optionalFieldWithDefaultWith', but without documentation.
optionalFieldWithDefaultWith' ::
  -- | Key
  Text ->
  -- | Codec for the value
  ValueCodec output output ->
  -- | Default value
  output ->
  ObjectCodec output output
optionalFieldWithDefaultWith' key c defaultValue = OptionalKeyWithDefaultCodec key c defaultValue Nothing

-- | An optional, or null, field
--
-- During decoding, the field may be in the object. 'Nothing' will be parsed if it is not.
-- If the field is @null@, then it will be parsed as 'Nothing' as well.
--
-- During encoding, the field will be in the object if it is not 'Nothing', and omitted otherwise.
optionalFieldOrNullWith ::
  -- | Key
  Text ->
  -- | Codec for the value
  ValueCodec input output ->
  -- | Documentation
  Text ->
  ObjectCodec (Maybe input) (Maybe output)
optionalFieldOrNullWith key c doc = orNullHelper $ OptionalKeyCodec key (maybeCodec c) (Just doc)

-- | Like 'optionalFieldOrNullWith', but without documentation
optionalFieldOrNullWith' ::
  -- | Key
  Text ->
  -- | Codec for the value
  ValueCodec input output ->
  ObjectCodec (Maybe input) (Maybe output)
optionalFieldOrNullWith' key c = orNullHelper $ OptionalKeyCodec key (maybeCodec c) Nothing

-- | Add a comment to a codec
--
-- This is an infix version of 'CommentCodec'
-- > (<?>) = flip CommentCodec
(<?>) ::
  ValueCodec input output ->
  -- | Comment
  Text ->
  ValueCodec input output
(<?>) = flip CommentCodec

-- | A version of '<?>' that lets you supply a list of lines of text instead of a single text.
--
-- This helps when you use an automated formatter that deals with lists more nicely than with multi-line strings.
(<??>) ::
  ValueCodec input output ->
  -- | Lines of comments
  [Text] ->
  ValueCodec input output
(<??>) c ls = CommentCodec (T.unlines ls) c

-- | Forward-compatible version of 'ValueCodec'
--
-- > valueCodec = ValueCodec
--
-- This is essentially your escape-hatch for when you would normally need a monad instance for 'Codec'.
-- You can build monad parsing by using 'valueCodec' together with 'bimapCodec' and supplying your own parsing function.
--
-- Note that this _does_ mean that the documentation will just say that you are parsing and rendering a value, so you may want to document the extra parsing further using '<?>'.
valueCodec :: JSONCodec JSON.Value
valueCodec = ValueCodec

-- | Forward-compatible version of 'NullCodec'
--
-- > nullCodec = NullCodec
--
-- === Example usage
--
-- >>> toJSONVia nullCodec ()
-- Null
nullCodec :: JSONCodec ()
nullCodec = NullCodec

-- | Forward-compatible version of 'BoolCodec' without a name
--
-- > boolCodec = BoolCodec Nothing
--
-- === Example usage
--
-- >>> toJSONVia boolCodec True
-- Bool True
boolCodec :: JSONCodec Bool
boolCodec = BoolCodec Nothing

-- | Forward-compatible version of 'StringCodec' without a name
--
-- > textCodec = StringCodec Nothing
--
-- === Example usage
--
-- >>> toJSONVia textCodec "hello"
-- String "hello"
textCodec :: JSONCodec Text
textCodec = StringCodec Nothing

-- | A 'String' version of 'textCodec'.
--
-- === Example usage
--
-- >>> toJSONVia stringCodec "hello"
-- String "hello"
--
-- === WARNING
--
-- This codec uses 'T.unpack' and 'T.pack' to dimap a 'textCodec', so it DOES NOT ROUNDTRIP.
--
-- >>> toJSONVia stringCodec "\55296"
-- String "\65533"
stringCodec :: JSONCodec String
stringCodec = dimapCodec T.unpack T.pack textCodec

-- | Forward-compatible version of 'NumberCodec' without a name
--
-- === Example usage
--
-- > scientificCodec = NumberCodec Nothing
--
-- >>> toJSONVia scientificCodec 5
-- Number 5.0
--
-- === WARNING
--
-- 'Scientific' is a type that is only for JSON parsing and rendering.
-- Do not use it for any calculations.
-- Instead, convert to another number type before doing any calculations.
--
-- >>> (1 / 3) :: Scientific
-- *** Exception: fromRational has been applied to a repeating decimal which can't be represented as a Scientific! It's better to avoid performing fractional operations on Scientifics and convert them to other fractional types like Double as early as possible.
-- CallStack (from HasCallStack):
--   error, called at src/Data/Scientific.hs:311:23 in scientific-0.3.6.2-19iaXRaHwRdEEucqiDAVk5:Data.Scientific
scientificCodec :: JSONCodec Scientific
scientificCodec = NumberCodec Nothing

-- | An object codec with a given name
--
-- > object name = ObjectOfCodec (Just name)
--
-- === Example usage
--
-- > data Example = Example
-- >   { exampleText :: !Text,
-- >     exampleBool :: !Bool
-- >   }
-- >
-- > instance HasCodec Example where
-- >   codec =
-- >     object "Example" $
-- >       Example
-- >         <$> requiredField "text" "a text" .= exampleText
-- >         <*> requiredField "bool" "a bool" .= exampleBool
object :: Text -> ObjectCodec input output -> ValueCodec input output
object name = ObjectOfCodec (Just name)

-- | A codec for bounded integers like 'Int', 'Int8', and 'Word'.
--
-- === WARNING
--
-- This codec will fail to parse numbers that are too big, for security reasons.
--
-- >>> JSON.parseEither( parseJSONVia boundedIntegerCodec) (Number 1e100) :: Either String Int
-- Left "Error in $: Number too big: 1.0e100"
boundedIntegerCodec :: (Integral i, Bounded i) => JSONCodec i
boundedIntegerCodec = bimapCodec go fromIntegral $ NumberCodec Nothing
  where
    go s = case Scientific.toBoundedInteger s of
      Nothing -> Left $ "Number did not fit into bounded integer: " <> show s
      Just i -> Right i

-- | A codec for a literal piece of 'Text'.
--
-- === Example usage
--
-- >>> toJSONVia (literalText "hello") "hello"
-- String "hello"
literalText :: Text -> JSONCodec Text
literalText text = EqCodec text textCodec

-- | A codec for a literal value corresponding to a literal piece of 'Text'.
--
-- === Example usage
--
-- >>> toJSONVia (literalTextValue True "yes") True
-- String "yes"
literalTextValue :: value -> Text -> JSONCodec value
literalTextValue value text = dimapCodec (const value) (const text) (literalText text)

-- |
--
-- TODO docs, example and footguns
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

-- |
--
-- TODO docs, example  and footguns
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

-- | A codec for an enum that can be written as constant string values>
--
-- === Example usage
--
-- > data Fruit = Apple | Orange
-- >  deriving (Show, Eq)
-- >
-- > instance HasCodec Fruit
-- >   codec = stringConstCodec [(Apple, "foo"), (Orange, "bar")]
-- >
--
-- >>> toJSONVia shownBoundedEnumCodec Orange
-- String "bar"
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
--
-- === Example usage
--
-- > data Fruit = Apple | Orange
-- >  deriving (Show, Eq, Enum, Bounded)
-- >
-- > instance HasCodec Fruit
-- >   codec = shownBoundedEnumCodec
-- >
--
-- >>> toJSONVia shownBoundedEnumCodec Apple
-- String "Apple"
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

-- | Helper function for 'optionalFieldOrNullWith' and 'optionalFieldOrNull'.
--
-- You probably don't need this.
orNullHelper ::
  ObjectCodec (Maybe (Maybe input)) (Maybe (Maybe output)) ->
  ObjectCodec (Maybe input) (Maybe output)
orNullHelper = dimapCodec f g
  where
    f :: Maybe (Maybe input) -> Maybe input
    f = \case
      Nothing -> Nothing
      Just Nothing -> Nothing
      Just (Just a) -> Just a
    g :: Maybe output -> Maybe (Maybe output)
    g = \case
      Nothing -> Nothing
      Just a -> Just (Just a)

-- | Forward-compatible version of 'ReferenceCodec'
--
-- > named = ReferenceCodec
named :: Text -> ValueCodec input output -> ValueCodec input output
named = ReferenceCodec
