{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Codec where

import Control.Monad.State
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
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
import Data.Validity
import Data.Validity.Scientific ()
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

-- $setup
-- >>> import Autodocodec.Aeson (toJSONVia, parseJSONVia)
-- >>> import Autodocodec.Class (HasCodec(codec))
-- >>> import qualified Data.Aeson as JSON
-- >>> import Data.Aeson (Value(..))
-- >>> import qualified Data.Vector as Vector
-- >>> import Data.Int
-- >>> import Data.Word
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists

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
-- * The @input@ parameter is used for the type that is used during encoding of a value, so it's the @input@ to the codec.
-- * The @output@ parameter is used for the type that is used during decoding of a value, so it's the @output@ of the codec.
-- * Both parameters are unused during documentation.
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
  -- The number has optional 'NumberBounds'.
  -- These are only enforced at decoding time, not at encoding-time.
  --
  -- NOTE: We use 'Scientific' here because that is what aeson uses.
  NumberCodec ::
    -- | Name of the @number@, for error messages and documentation.
    (Maybe Text) ->
    -- | Bounds for the number, these are checked and documented
    Maybe NumberBounds ->
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
  --
  -- Note that this codec works for both values and objects.
  -- However: due to the complex nature of documentation, the documentation may
  -- not be as good as you would hope when you use this codec.
  -- In particular, you should prefer using it for values rather than objects,
  -- because those docs are easier to generate.
  EitherCodec ::
    -- | Codec for the 'Left' side
    (Codec context input1 output1) ->
    -- | Codec for the 'Right' side
    (Codec context input2 output2) ->
    -- |
    Codec context (Either input1 input2) (Either output1 output2)
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
  OptionalKeyWithOmittedDefaultCodec ::
    Eq value =>
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

data NumberBounds = NumberBounds
  { numberBoundsLower :: !Scientific,
    numberBoundsUpper :: !Scientific
  }
  deriving (Show, Eq, Generic)

instance Validity NumberBounds

checkNumberBounds :: NumberBounds -> Scientific -> Either String Scientific
checkNumberBounds NumberBounds {..} s =
  if numberBoundsLower <= s
    then
      if s <= numberBoundsUpper
        then Right s
        else Left $ unwords ["Number", show s, "is bigger than the upper bound", show numberBoundsUpper]
    else Left $ unwords ["Number", show s, "is smaller than the lower bound", show numberBoundsUpper]

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

-- | A completed autodocodec for parsing and rendering a 'JSON.Value'.
--
-- You can use a value of this type to get everything else for free:
--
-- * Encode values to JSON using 'toJSONViaCodec' or 'toJSONVia'
-- * Decode values from JSON using 'parseJSONViaCodec' or 'parseJSONVia'
-- * Produce a JSON Schema using 'jsonSchemaViaCodec' or 'jsonSchemaVia' from @autodocodec-schema@
-- * Encode to and decode from Yaml using @autodocodec-yaml@
-- * Produce a human-readible YAML schema using @renderColouredSchemaViaCodec@ from @autodocodec-yaml@
-- * Produce a Swagger2 schema using @autodocodec-swagger2@
-- * Produce a OpenAPI3 schema using @autodocodec-openapi3@
type JSONCodec a = ValueCodec a a

-- | A completed autodocodec for parsing and rendering a 'JSON.Object'.
type JSONObjectCodec a = ObjectCodec a a

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
      NumberCodec mName mbs -> pure $ showParen (d > 10) $ showString "NumberCodec " . showsPrec 11 mName . showString " " . showsPrec 11 mbs
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
      OptionalKeyWithOmittedDefaultCodec k c _ mdoc -> (\s -> showParen (d > 10) $ showString "OptionalKeyWithOmittedDefaultCodec " . showsPrec 11 k . showString " " . s . showString " _ " . showsPrec 11 mdoc) <$> go 11 c
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
  Codec context input1 output1 ->
  Codec context input2 output2 ->
  Codec context (Either input1 input2) (Either output1 output2)
eitherCodec = EitherCodec

-- | Map a codec's input and output types.
--
-- This function allows you to have the parsing fail in a new way.
--
-- If you use this function, then you will most likely want to add documentation about how not every value that the schema specifies will be accepted.
--
-- This function is like 'BimapCodec' except it also combines one level of a nested 'BimapCodec's.
--
-- === Example usage
--
-- logLevelCodec :: JSONCodec LogLevel
-- logLevelCodec = bimapCodec parseLogLevel renderLogLevel codec <?> "Valid values include DEBUG, INFO, WARNING, ERROR."
bimapCodec ::
  (oldOutput -> Either String newOutput) ->
  (newInput -> oldInput) ->
  Codec context oldInput oldOutput ->
  Codec context newInput newOutput
bimapCodec f g = \case
  BimapCodec f' g' c -> BimapCodec (f' >=> f) (g' . g) c
  c -> BimapCodec f g c

-- | Forward-compatible version of 'ArrayOfCodec' without a name
--
-- > vectorCodec = ArrayOfCodec Nothing
--
-- === Example usage
--
-- >>> toJSONVia (vectorCodec codec) (Vector.fromList ['a','b'])
-- Array [String "a",String "b"]
vectorCodec :: ValueCodec input output -> ValueCodec (Vector input) (Vector output)
vectorCodec = ArrayOfCodec Nothing

-- | Build a codec for lists of values from a codec for a single value.
--
-- === Example usage
--
-- >>> toJSONVia (listCodec codec) ['a','b']
-- Array [String "a",String "b"]
listCodec :: ValueCodec input output -> ValueCodec [input] [output]
listCodec = dimapCodec V.toList V.fromList . vectorCodec

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
-- During encoding, the field will be omitted from the object if it is 'Nothing'.
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
-- During encoding, the field will always be in the object. The default value is ignored.
--
-- The shown version of the default value will appear in the documentation.
optionalFieldWithDefaultWith ::
  -- | Key
  Text ->
  -- | Codec for the value
  JSONCodec output ->
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
  JSONCodec output ->
  -- | Default value
  output ->
  ObjectCodec output output
optionalFieldWithDefaultWith' key c defaultValue = OptionalKeyWithDefaultCodec key c defaultValue Nothing

-- | An optional field with default value that can be omitted when encoding
--
-- During decoding, the field may be in the object. The default value will be parsed otherwise.
--
-- During encoding, the field will be omitted from the object if it is equal to the default value.
--
-- The shown version of the default value will appear in the documentation.
optionalFieldWithOmittedDefaultWith ::
  Eq output =>
  -- | Key
  Text ->
  -- | Codec for the value
  JSONCodec output ->
  -- | Default value
  output ->
  -- | Documentation
  Text ->
  ObjectCodec output output
optionalFieldWithOmittedDefaultWith key c defaultValue doc = OptionalKeyWithOmittedDefaultCodec key c defaultValue (Just doc)

-- | Like 'optionalFieldWithOmittedDefaultWith', but without documentation.
optionalFieldWithOmittedDefaultWith' ::
  Eq output =>
  -- | Key
  Text ->
  -- | Codec for the value
  JSONCodec output ->
  -- | Default value
  output ->
  ObjectCodec output output
optionalFieldWithOmittedDefaultWith' key c defaultValue = OptionalKeyWithOmittedDefaultCodec key c defaultValue Nothing

optionalFieldOrNullWithOmittedDefaultWith ::
  Eq output =>
  -- | Key
  Text ->
  -- | Codec for the value
  JSONCodec output ->
  -- | Default value
  output ->
  -- | Documentation
  Text ->
  ObjectCodec output output
optionalFieldOrNullWithOmittedDefaultWith key c defaultValue doc = dimapCodec f g $ optionalFieldWithOmittedDefaultWith key (maybeCodec c) (Just defaultValue) doc
  where
    f = \case
      Just v -> v
      Nothing -> defaultValue
    g v = if v == defaultValue then Nothing else Just v

optionalFieldOrNullWithOmittedDefaultWith' ::
  Eq output =>
  -- | Key
  Text ->
  -- | Codec for the value
  JSONCodec output ->
  -- | Default value
  output ->
  ObjectCodec output output
optionalFieldOrNullWithOmittedDefaultWith' key c defaultValue = dimapCodec f g $ optionalFieldWithOmittedDefaultWith' key (maybeCodec c) (Just defaultValue)
  where
    f = \case
      Just v -> v
      Nothing -> defaultValue
    g v = if v == defaultValue then Nothing else Just v

-- | An optional, or null, field
--
-- During decoding, the field may be in the object. 'Nothing' will be parsed if it is not.
-- If the field is @null@, then it will be parsed as 'Nothing' as well.
--
-- During encoding, the field will be omitted from the object if it is 'Nothing'.
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
-- >>> JSON.parseMaybe (parseJSONVia nullCodec) Null
-- Just ()
-- >>> JSON.parseMaybe (parseJSONVia nullCodec) (Number 5)
-- Nothing
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
-- This codec uses 'T.unpack' and 'T.pack' to dimap a 'textCodec', so it __does not roundtrip__.
--
-- >>> toJSONVia stringCodec "\55296"
-- String "\65533"
stringCodec :: JSONCodec String
stringCodec = dimapCodec T.unpack T.pack textCodec

-- | Forward-compatible version of 'NumberCodec' without a name
--
-- === Example usage
--
-- > scientificCodec = NumberCodec Nothing Nothing
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
-- @
-- λ> (1 / 3) :: Scientific
-- *** Exception: fromRational has been applied to a repeating decimal which can't be represented as a Scientific! It's better to avoid performing fractional operations on Scientifics and convert them to other fractional types like Double as early as possible.
-- @
scientificCodec :: JSONCodec Scientific
scientificCodec = NumberCodec Nothing Nothing

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
-- This codec will not have a name, and it will use the 'boundedNumberBounds' to add number bounds.
--
-- >>> let c = boundedIntegralCodec :: JSONCodec Int8
-- >>> toJSONVia c 5
-- Number 5.0
-- >>> JSON.parseMaybe (parseJSONVia c) (Number 100)
-- Just 100
-- >>> JSON.parseMaybe (parseJSONVia c) (Number 200)
-- Nothing
boundedIntegralCodec :: forall i. (Integral i, Bounded i) => JSONCodec i
boundedIntegralCodec =
  bimapCodec go fromIntegral $
    NumberCodec
      Nothing
      ( Just (boundedIntegralNumberBounds @i)
      )
  where
    go s = case Scientific.toBoundedInteger s of
      Nothing -> Left $ "Number did not fit into bounded integer: " <> show s
      Just i -> Right i

-- | 'NumberBounds' for a bounded integral type.
--
-- You can call this using @TypeApplications@: @boundedIntegralNumberBounds @Word@
boundedIntegralNumberBounds :: forall i. (Integral i, Bounded i) => NumberBounds
boundedIntegralNumberBounds =
  NumberBounds
    { numberBoundsLower = fromIntegral (minBound :: i),
      numberBoundsUpper = fromIntegral (maxBound :: i)
    }

-- | A codec for a literal piece of 'Text'.
--
-- During parsing, only the given 'Text' is accepted.
--
-- During rendering, the given 'Text' is always output.
--
-- === Example usage
--
-- >>> let c = literalTextCodec "hello"
-- >>> toJSONVia c "hello"
-- String "hello"
-- >>> toJSONVia c "world"
-- String "hello"
-- >>> JSON.parseMaybe (parseJSONVia c) (String "hello")
-- Just "hello"
-- >>> JSON.parseMaybe (parseJSONVia c) (String "world")
-- Nothing
literalTextCodec :: Text -> JSONCodec Text
literalTextCodec text = EqCodec text textCodec

-- | A codec for a literal value corresponding to a literal piece of 'Text'.
--
-- During parsing, only the given 'Text' is accepted.
--
-- During rendering, the given @value@ is always output.
--
-- === Example usage
--
-- >>> let c = literalTextValueCodec True "yes"
-- >>> toJSONVia c True
-- String "yes"
-- >>> toJSONVia c False
-- String "yes"
-- >>> JSON.parseMaybe (parseJSONVia c) (String "yes") :: Maybe Bool
-- Just True
-- >>> JSON.parseMaybe (parseJSONVia c) (String "no") :: Maybe Bool
-- Nothing
literalTextValueCodec :: value -> Text -> JSONCodec value
literalTextValueCodec value text = dimapCodec (const value) (const text) (literalTextCodec text)

-- | A choice codec, but unlike 'eitherCodec', it's for the same type instead of different ones.
--
-- While parsing, this codec will first try the left codec, then the right if that fails.
--
-- While rendering, the provided function is used to decide which codec to use for rendering.
--
-- Note: The reason this is less primitive than the 'eitherCodec' is that 'Either' makes it clear which codec you want to use for rendering.
-- In this case, we need to provide our own function for choosing which codec we want to use for rendering.
--
-- === Example usage
--
-- >>> :{
--   let c =
--        matchChoiceCodec
--         (literalTextCodec "even")
--         (literalTextCodec "odd")
--         (\s -> if s == "even" then Left s else Right s)
-- :}
--
-- >>> toJSONVia c "even"
-- String "even"
-- >>> toJSONVia c "odd"
-- String "odd"
-- >>> JSON.parseMaybe (parseJSONVia c) (String "even") :: Maybe Text
-- Just "even"
-- >>> JSON.parseMaybe (parseJSONVia c) (String "odd") :: Maybe Text
-- Just "odd"
matchChoiceCodec ::
  -- | First codec
  Codec context input output ->
  -- | Second codec
  Codec context input output ->
  -- | Rendering chooser
  (newInput -> Either input input) ->
  -- |
  Codec context newInput output
matchChoiceCodec c1 c2 renderingChooser =
  dimapCodec f renderingChooser $
    eitherCodec c1 c2
  where
    f = \case
      Left a -> a
      Right a -> a

-- | A choice codec for a list of options, each with their own rendering matcher.
--
-- During parsing, each of the codecs are tried from first to last until one succeeds.
--
-- During rendering, each matching function is tried until either one succeeds and the corresponding codec is used, or none succeed and the fallback codec is used.
--
-- === Example usage
--
-- >>> :{
--   let c =
--        matchChoicesCodec
--          [ (\s -> if s == "even" then Just s else Nothing, literalTextCodec "even")
--          , (\s -> if s == "odd" then Just s else Nothing, literalTextCodec "odd")
--          ] (literalTextCodec "fallback")
-- :}
--
-- >>> toJSONVia c "even"
-- String "even"
-- >>> toJSONVia c "odd"
-- String "odd"
-- >>> toJSONVia c "foobar"
-- String "fallback"
-- >>> JSON.parseMaybe (parseJSONVia c) (String "even") :: Maybe Text
-- Just "even"
-- >>> JSON.parseMaybe (parseJSONVia c) (String "odd") :: Maybe Text
-- Just "odd"
-- >>> JSON.parseMaybe (parseJSONVia c) (String "foobar") :: Maybe Text
-- Nothing
-- >>> JSON.parseMaybe (parseJSONVia c) (String "fallback") :: Maybe Text
-- Just "fallback"
matchChoicesCodec ::
  -- | Codecs, each which their own rendering matcher
  [(input -> Maybe input, Codec context input output)] ->
  -- | Fallback codec, in case none of the matchers in the list match
  Codec context input output ->
  -- |
  Codec context input output
matchChoicesCodec l fallback = go l
  where
    go = \case
      [] -> fallback
      ((m, c) : rest) -> matchChoiceCodec c (go rest) $ \i -> case m i of
        Just j -> Left j
        Nothing -> Right i

-- | Use one codec for the default way of parsing and rendering, but then also
-- use a list of other codecs for potentially different parsing.
--
-- You can use this for keeping old ways of parsing intact while already rendering in the new way.
--
-- === Example usage
--
-- >>> data Fruit = Apple | Orange deriving (Show, Eq, Bounded, Enum)
-- >>> let c = parseAlternatives shownBoundedEnumCodec [stringConstCodec [(Apple, "foo"), (Orange, "bar")]]
-- >>> toJSONVia c Apple
-- String "Apple"
-- >>> JSON.parseMaybe (parseJSONVia c) (String "foo") :: Maybe Fruit
-- Just Apple
-- >>> JSON.parseMaybe (parseJSONVia c) (String "Apple") :: Maybe Fruit
-- Just Apple
parseAlternatives ::
  -- | Main codec, for parsing and rendering
  Codec context input output ->
  -- | Alternative codecs just for parsing
  [Codec context input output] ->
  Codec context input output
parseAlternatives c rest = go (c :| rest)
  where
    go :: NonEmpty (Codec context input output) -> Codec context input output
    go = \case
      (c' :| cRest) -> case NE.nonEmpty cRest of
        Nothing -> c'
        Just ne' -> matchChoiceCodec c' (go ne') Left

-- | Like 'parseAlternatives', but with only one alternative codec
--
-- === Example usage
--
-- >>> data Fruit = Apple | Orange deriving (Show, Eq, Bounded, Enum)
-- >>> let c = parseAlternative shownBoundedEnumCodec (stringConstCodec [(Apple, "foo"), (Orange, "bar")])
-- >>> toJSONVia c Apple
-- String "Apple"
-- >>> JSON.parseMaybe (parseJSONVia c) (String "foo") :: Maybe Fruit
-- Just Apple
-- >>> JSON.parseMaybe (parseJSONVia c) (String "Apple") :: Maybe Fruit
-- Just Apple
parseAlternative ::
  -- | Main codec, for parsing and rendering
  Codec context input output ->
  -- | Alternative codecs just for parsing
  Codec context input output ->
  Codec context input output
parseAlternative c cAlt = parseAlternatives c [cAlt]

-- | A codec for an enum that can be written each with their own codec.
--
-- === WARNING
--
-- If you don't provide a string for one of the type's constructors, the last codec in the list will be used instead.
enumCodec ::
  forall enum context.
  Eq enum =>
  -- |
  NonEmpty (enum, Codec context enum enum) ->
  -- |
  Codec context enum enum
enumCodec = go
  where
    go :: NonEmpty (enum, Codec context enum enum) -> Codec context enum enum
    go ((e, c) :| rest) = case NE.nonEmpty rest of
      Nothing -> c
      Just ne -> matchChoiceCodec c (go ne) $ \i ->
        if e == i
          then Left e
          else Right i

-- | A codec for an enum that can be written as constant string values
--
-- === Example usage
--
-- >>> data Fruit = Apple | Orange deriving (Show, Eq)
-- >>> let c = stringConstCodec [(Apple, "foo"), (Orange, "bar")]
-- >>> toJSONVia c Orange
-- String "bar"
-- >>> JSON.parseMaybe (parseJSONVia c) (String "foo") :: Maybe Fruit
-- Just Apple
--
-- === WARNING
--
-- If you don't provide a string for one of the type's constructors, the last string in the list will be used instead:
--
-- >>> let c = stringConstCodec [(Apple, "foo")]
-- >>> toJSONVia c Orange
-- String "foo"
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
            literalTextValueCodec constant text
          )
      )

-- | A codec for a 'Bounded' 'Enum' that uses its 'Show' instance to have the values correspond to literal 'Text' values.
--
-- === Example usage
--
-- >>> data Fruit = Apple | Orange deriving (Show, Eq, Enum, Bounded)
-- >>> let c = shownBoundedEnumCodec
-- >>> toJSONVia c Apple
-- String "Apple"
-- >>> JSON.parseMaybe (parseJSONVia c) (String "Orange") :: Maybe Fruit
-- Just Orange
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

-- | Name a codec.
--
-- This is used to allow for references to the codec, and that's necessary
-- to produce finite documentation for recursive codecs.
--
-- Forward-compatible version of 'ReferenceCodec'
--
-- > named = ReferenceCodec
named :: Text -> ValueCodec input output -> ValueCodec input output
named = ReferenceCodec

-- | Produce a codec using a type's 'FromJSON' and 'ToJSON' instances.
--
-- You will only want to use this if you cannot figure out how to produce a
-- 'JSONCodec' for your type.
--
-- Note that this will not have good documentation because, at a codec level,
-- it's just parsing and rendering a 'JSON.Value'.
--
-- === Example usage
--
-- >>> toJSONVia (codecViaAeson "Int") (5 :: Int)
-- Number 5.0
-- >>> JSON.parseMaybe (parseJSONVia (codecViaAeson "Int")) (Number 5) :: Maybe Int
-- Just 5
codecViaAeson ::
  (FromJSON a, ToJSON a) =>
  -- | Name
  Text ->
  JSONCodec a
codecViaAeson doc = bimapCodec (JSON.parseEither JSON.parseJSON) JSON.toJSON valueCodec <?> doc
