{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
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

import Control.Monad
import Control.Monad.State
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson as JSON
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
#endif
import qualified Data.Aeson.Types as JSON
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.List (intersperse)
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
import Data.Void
import GHC.Generics (Generic)

-- $setup
-- >>> import Autodocodec.Aeson (toJSONVia, toJSONViaCodec, parseJSONVia, parseJSONViaCodec)
-- >>> import qualified Autodocodec.Aeson.Compat as Compat
-- >>> import Autodocodec.Class (HasCodec(codec), requiredField)
-- >>> import qualified Data.Aeson as JSON
-- >>> import qualified Data.HashMap.Strict as HM
-- >>> import Data.Aeson (Value(..))
-- >>> import qualified Data.Vector as Vector
-- >>> import Data.Int
-- >>> import Data.Word
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> :set -XLambdaCase

-- | A Self-documenting encoder and decoder,
--
-- also called an "Autodocodec".
--
-- In an ideal situation, this type would have only one type parameter: 'Codec value'.
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
    ValueCodec () ()
  -- | Encode a 'Bool' to a @boolean@ value, and decode a @boolean@ value as a 'Bool'.
  BoolCodec ::
    -- | Name of the @bool@, for error messages and documentation.
    Maybe Text ->
    JSONCodec Bool
  -- | Encode 'Text' to a @string@ value, and decode a @string@ value as a 'Text'.
  --
  -- This is named after the primitive type "String" in json, not after the haskell type string.
  StringCodec ::
    -- | Name of the @string@, for error messages and documentation.
    Maybe Text ->
    JSONCodec Text
  -- | Encode 'Scientific' to a @number@ value, and decode a @number@ value as a 'Scientific'.
  --
  -- The number has optional 'NumberBounds'.
  -- These are only enforced at decoding time, not at encoding-time.
  --
  -- NOTE: We use 'Scientific' here because that is what aeson uses.
  NumberCodec ::
    -- | Name of the @number@, for error messages and documentation.
    Maybe Text ->
    -- | Bounds for the number, these are checked and documented
    Maybe NumberBounds ->
    JSONCodec Scientific
  -- | Encode a 'HashMap', and decode any 'HashMap'.
  HashMapCodec ::
    (Eq k, Hashable k, FromJSONKey k, ToJSONKey k) =>
    JSONCodec v ->
    JSONCodec (HashMap k v)
  -- | Encode a 'Map', and decode any 'Map'.
  MapCodec ::
    (Ord k, FromJSONKey k, ToJSONKey k) =>
    JSONCodec v ->
    JSONCodec (Map k v)
  -- | Encode a 'JSON.Value', and decode any 'JSON.Value'.
  ValueCodec ::
    JSONCodec JSON.Value
  -- | Encode a 'Vector' of values as an @array@ value, and decode an @array@ value as a 'Vector' of values.
  ArrayOfCodec ::
    -- | Name of the @array@, for error messages and documentation.
    Maybe Text ->
    ValueCodec input output ->
    ValueCodec (Vector input) (Vector output)
  -- | Encode a value as a an @object@ value using the given 'ObjectCodec', and decode an @object@ value as a value using the given 'ObjectCodec'.
  ObjectOfCodec ::
    -- | Name of the @object@, for error messages and documentation.
    Maybe Text ->
    ObjectCodec input output ->
    ValueCodec input output
  -- | Match a given value using its 'Eq' instance during decoding, and encode exactly that value during encoding.
  EqCodec ::
    (Show value, Eq value) =>
    -- | Value to match
    value ->
    -- | Codec for the value
    JSONCodec value ->
    JSONCodec value
  -- | Map a codec in both directions.
  --
  -- This is not strictly dimap, because the decoding function is allowed to fail,
  -- but we can implement dimap using this function by using a decoding function that does not fail.
  -- Otherwise we would have to have another constructor here.
  BimapCodec ::
    (oldOutput -> Either String newOutput) ->
    (newInput -> oldInput) ->
    Codec context oldInput oldOutput ->
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
    -- | What type of union we encode and decode
    !Union ->
    -- | Codec for the 'Left' side
    Codec context input1 output1 ->
    -- | Codec for the 'Right' side
    Codec context input2 output2 ->
    Codec context (Either input1 input2) (Either output1 output2)
  -- | Encode/decode a discriminated union of objects
  --
  -- The type of object being encoded/decoded is discriminated by
  -- a designated "discriminator" property on the object which takes a string value.
  --
  -- When encoding, the provided function is applied to the input to obtain a new encoder
  -- for the input. The function 'mapToEncoder' is provided to assist with building these
  -- encoders.
  --
  -- When decoding, the value of the discriminator property is looked up in the `HashMap`
  -- to obtain a decoder for the output. The function `mapToDecoder' is provided
  -- to assist with building these decoders. See examples in 'Usage.hs'.
  --
  -- The 'HashMap' is also used to generate schemas for the type.
  -- In particular, for OpenAPI 3, it will generate a schema with a 'discriminator', as defined
  -- by https://swagger.io/docs/specification/data-models/inheritance-and-polymorphism/
  DiscriminatedUnionCodec ::
    -- | propertyName to use for discrimination
    Text ->
    -- | how to encode the input
    (input -> (Discriminator, ObjectCodec input ())) ->
    -- | how to decode the output
    -- The 'Text' field is the name to use for the object schema.
    HashMap Discriminator (Text, ObjectCodec Void output) ->
    ObjectCodec input output
  -- | A comment codec
  --
  -- This is used to add implementation-irrelevant but human-relevant information.
  CommentCodec ::
    -- | Comment
    Text ->
    ValueCodec input output ->
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
    ObjectCodec input output
  OptionalKeyCodec ::
    -- | Key
    Text ->
    -- | Codec for the value
    ValueCodec input output ->
    -- | Documentation
    Maybe Text ->
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
    ObjectCodec value value
  -- | To implement 'pure' from 'Applicative'.
  --
  -- Pure is not available for non-object codecs because there is no 'mempty' for 'JSON.Value', which we would need during encoding.
  PureCodec ::
    output ->
    -- |
    --
    -- We have to use 'void' instead of 'Void' here to be able to implement 'Applicative'.
    ObjectCodec void output
  -- | To implement '<*>' from 'Applicative'.
  --
  -- Ap is not available for non-object codecs because we cannot combine ('mappend') two encoded 'JSON.Value's
  ApCodec ::
    ObjectCodec input (output -> newOutput) ->
    ObjectCodec input output ->
    ObjectCodec input newOutput

data NumberBounds = NumberBounds
  { numberBoundsLower :: !Scientific,
    numberBoundsUpper :: !Scientific
  }
  deriving (Show, Eq, Generic)

instance Validity NumberBounds

-- | Check if a number falls within given 'NumberBounds'.
checkNumberBounds :: NumberBounds -> Scientific -> Either String Scientific
checkNumberBounds NumberBounds {..} s =
  if numberBoundsLower <= s
    then
      if s <= numberBoundsUpper
        then Right s
        else Left $ unwords ["Number", show s, "is bigger than the upper bound", show numberBoundsUpper]
    else Left $ unwords ["Number", show s, "is smaller than the lower bound", show numberBoundsUpper]

-- | What type of union the encoding uses
data Union
  = -- | Not disjoint, see 'possiblyJointEitherCodec'.
    PossiblyJointUnion
  | -- | Disjoint, see 'disjointEitherCodec'.
    DisjointUnion
  deriving (Show, Eq, Generic)

instance Validity Union

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
      EitherCodec u c1 c2 -> (\s1 s2 -> showParen (d > 10) $ showString "EitherCodec " . showsPrec 11 u . showString " " . s1 . showString " " . s2) <$> go 11 c1 <*> go 11 c2
      DiscriminatedUnionCodec propertyName _ mapping -> do
        cs <- traverse (\(n, (_, c)) -> (\s -> showParen True $ shows n . showString ", " . s) <$> go 11 c) $ HashMap.toList mapping
        let csList = showString "[" . foldr (.) id (intersperse (showString ", ") cs) . showString "]"
        pure $ showParen (d > 10) $ showString "DiscriminatedUnionCodec " . showsPrec 11 propertyName . showString " _ " . csList
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
--
-- === 'HasCodec' instance for newtypes
--
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

-- | Produce a value without parsing any part of an 'Object'.
--
-- This function exists to implement @Applicative (ObjectCodec input)@.
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'PureCodec'.
--
-- > pureCodec = PureCodec
pureCodec :: output -> ObjectCodec input output
pureCodec = PureCodec

-- | Sequentially apply two codecs that parse part of an 'Object'.
--
-- This function exists to implement @Applicative (ObjectCodec input)@.
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'ApCodec'.
--
-- > apCodec = ApCodec
apCodec :: ObjectCodec input (output -> newOutput) -> ObjectCodec input output -> ObjectCodec input newOutput
apCodec = ApCodec

instance Applicative (ObjectCodec input) where
  pure = pureCodec
  (<*>) = apCodec

-- | Maybe codec
--
-- This can be used to also allow @null@ during decoding of a 'Maybe' value.
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
maybeCodec =
  -- We must use 'possiblyJointEitherCodec' here, otherwise a codec for (Maybe
  -- (Maybe Text)) will fail to parse.
  dimapCodec f g
    . possiblyJointEitherCodec nullCodec
  where
    f = \case
      Left () -> Nothing
      Right r -> Just r
    g = \case
      Nothing -> Left ()
      Just r -> Right r

-- | Either codec
--
-- During encoding, parse a value according to either codec.
-- During encoding, use the corresponding codec to encode either value.
--
-- === 'HasCodec' instance for sum types
--
-- To write a 'HasCodec' instance for sum types, you will need to decide whether encoding is disjoint or not.
-- The default, so also the implementation of this function, is 'possiblyJointEitherCodec', but you may want to use 'disjointEitherCodec' instead.
--
-- Ask yourself: Can the encoding of a 'Left' value be decoded as 'Right' value (or vice versa)?
--
-- @Yes ->@ use 'possiblyJointEitherCodec'.
--
-- @No  ->@ use 'disjointEitherCodec'.
--
--
-- === Example usage
--
-- >>> let c = eitherCodec codec codec :: JSONCodec (Either Int String)
-- >>> toJSONVia c (Left 5)
-- Number 5.0
-- >>> toJSONVia c (Right "hello")
-- String "hello"
-- >>> JSON.parseMaybe (parseJSONVia c) (String "world") :: Maybe (Either Int String)
-- Just (Right "world")
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'possiblyJointEitherCodec'.
--
-- > eitherCodec = possiblyJointEitherCodec
eitherCodec ::
  Codec context input1 output1 ->
  Codec context input2 output2 ->
  Codec context (Either input1 input2) (Either output1 output2)
eitherCodec = possiblyJointEitherCodec

-- | Possibly joint either codec
--
-- During encoding, parse a value according to either codec.
-- During encoding, use the corresponding codec to encode either value.
--
-- This codec is for the case in which parsing must be disjoint.
--
-- === 'HasCodec' instance for sum types with an encoding that is definitely disjoint.
--
-- The 'eitherCodec' can be used to implement 'HasCodec' instances for sum types
-- for which the encoding is definitely disjoint.
--
-- >>> data War = WorldWar Word8 | OtherWar Text deriving (Show, Eq)
-- >>> :{
--   instance HasCodec War where
--    codec =
--      dimapCodec f g $
--        disjointEitherCodec
--          (codec :: JSONCodec Word8)
--          (codec :: JSONCodec Text)
--      where
--        f = \case
--          Left w -> WorldWar w
--          Right t -> OtherWar t
--        g = \case
--          WorldWar w -> Left w
--          OtherWar t -> Right t
-- :}
--
-- Note that this incoding is indeed disjoint because an encoded 'String' can
-- never be parsed as an 'Word8' and vice versa.
--
-- >>> toJSONViaCodec (WorldWar 2)
-- Number 2.0
-- >>> toJSONViaCodec (OtherWar "OnDrugs")
-- String "OnDrugs"
-- >>> JSON.parseMaybe parseJSONViaCodec (String "of the roses") :: Maybe War
-- Just (OtherWar "of the roses")
--
--
-- === WARNING
--
-- If it turns out that the encoding of a value is not disjoint, decoding may
-- fail and documentation may be wrong.
--
-- >>> let c = disjointEitherCodec (codec :: JSONCodec Int) (codec :: JSONCodec Int)
-- >>> JSON.parseMaybe (parseJSONVia c) (Number 5) :: Maybe (Either Int Int)
-- Nothing
--
-- Encoding still works as expected, however:
--
-- >>> toJSONVia c (Left 5)
-- Number 5.0
-- >>> toJSONVia c (Right 6)
-- Number 6.0
--
--
-- === Example usage
--
-- >>> toJSONVia (disjointEitherCodec (codec :: JSONCodec Int) (codec :: JSONCodec String)) (Left 5)
-- Number 5.0
-- >>> toJSONVia (disjointEitherCodec (codec :: JSONCodec Int) (codec :: JSONCodec String)) (Right "hello")
-- String "hello"
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'EitherCodec DisjointUnion'.
--
-- > disjointEitherCodec = EitherCodec DisjointUnion
disjointEitherCodec ::
  Codec context input1 output1 ->
  Codec context input2 output2 ->
  Codec context (Either input1 input2) (Either output1 output2)
disjointEitherCodec = EitherCodec DisjointUnion

-- | Possibly joint either codec
--
-- During encoding, parse a value according to either codec.
-- During encoding, use the corresponding codec to encode either value.
--
-- This codec is for the case in which parsing may not be disjoint.
--
-- === 'HasCodec' instance for sum types with an encoding that is not disjoint.
--
-- The 'eitherCodec' can be used to implement 'HasCodec' instances for sum types.
-- If you just have two codecs that you want to try in order, while parsing, you can do this:
--
-- >>> :{
--   data Ainur
--     = Valar Text Text
--     | Maiar Text
--     deriving (Show, Eq)
-- :}
--
-- >>> :{
--   instance HasCodec Ainur where
--     codec =
--       dimapCodec f g $
--         possiblyJointEitherCodec
--           (object "Valar" $
--             (,)
--              <$> requiredField "domain" "Domain which the Valar rules over" .= fst
--              <*> requiredField "name" "Name of the Valar" .= snd)
--           (object "Maiar" $ requiredField "name" "Name of the Maiar")
--       where
--         f = \case
--           Left (domain, name) -> Valar domain name
--           Right name -> Maiar name
--         g = \case
--           Valar domain name -> Left (domain, name)
--           Maiar name -> Right name
-- :}
--
-- Note that this encoding is indeed not disjoint, because a @Valar@ object can
-- parse as a @Maiar@ value.
--
-- >>> toJSONViaCodec (Valar "Stars" "Varda")
-- Object (fromList [("domain",String "Stars"),("name",String "Varda")])
-- >>> toJSONViaCodec (Maiar "Sauron")
-- Object (fromList [("name",String "Sauron")])
-- >>> JSON.parseMaybe parseJSONViaCodec (Object (Compat.fromList [("name",String "Olorin")])) :: Maybe Ainur
-- Just (Maiar "Olorin")
--
--
-- === WARNING
--
-- The order of the codecs in a 'possiblyJointEitherCodec' matters.
--
-- In the above example, decoding works as expected because the @Valar@ case is parsed first.
-- If the @Maiar@ case were first in the 'possiblyJointEitherCodec', then
-- @Valar@ could never be parsed.
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'EitherCodec PossiblyJointUnion'.
--
-- > possiblyJointEitherCodec = EitherCodec PossiblyJointUnion
possiblyJointEitherCodec ::
  Codec context input1 output1 ->
  Codec context input2 output2 ->
  Codec context (Either input1 input2) (Either output1 output2)
possiblyJointEitherCodec = EitherCodec PossiblyJointUnion

-- | Discriminator value used in 'DiscriminatedUnionCodec'
type Discriminator = Text

-- | Wrap up a value of type 'b' with its codec to produce
-- and encoder for 'a's that ignores its input and instead encodes
-- the value 'b'.
-- This is useful for building 'discriminatedUnionCodec's.
mapToEncoder :: b -> Codec context b any -> Codec context a ()
mapToEncoder b = dimapCodec (const ()) (const b)

-- | Map a codec for decoding 'b's into a decoder for 'a's.
-- This is useful for building 'discriminatedUnionCodec's.
mapToDecoder :: (b -> a) -> Codec context any b -> Codec context Void a
mapToDecoder f = dimapCodec f absurd

-- | Encode/decode a discriminated union of objects
--
-- The type of object being encoded/decoded is discriminated by
-- a designated "discriminator" property on the object which takes a string value.
--
-- When encoding, the provided function is applied to the input to obtain a new encoder
-- for the input. The function 'mapToEncoder' is provided to assist with building these
-- encoders. See examples in 'Usage.hs'.
--
-- When decoding, the value of the discriminator property is looked up in the `HashMap`
-- to obtain a decoder for the output. The function `mapToDecoder' is provided
-- to assist with building these decoders. See examples in 'Usage.hs'.
--
-- The 'HashMap' is also used to generate schemas for the type.
-- In particular, for OpenAPI 3, it will generate a schema with a 'discriminator', as defined
-- by https://swagger.io/docs/specification/data-models/inheritance-and-polymorphism/
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'DiscriminatedUnionCodec'.
--
-- > discriminatedUnionCodec = 'DiscriminatedUnionCodec'
discriminatedUnionCodec ::
  -- | propertyName
  Text ->
  -- | how to encode the input
  --
  -- Use 'mapToEncoder' to produce the 'ObjectCodec's.
  (input -> (Discriminator, ObjectCodec input ())) ->
  -- | how to decode the output
  --
  -- The 'Text' field is the name to use for the object schema.
  --
  -- Use 'mapToDecoder' to produce the 'ObjectCodec's.
  HashMap Discriminator (Text, ObjectCodec Void output) ->
  ObjectCodec input output
discriminatedUnionCodec = DiscriminatedUnionCodec

-- | Map a codec's input and output types.
--
-- This function allows you to have the parsing fail in a new way.
--
-- If you use this function, then you will most likely want to add documentation about how not every value that the schema specifies will be accepted.
--
-- This function is like 'BimapCodec' except it also combines one level of a nested 'BimapCodec's.
--
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
bimapCodec f g =
  -- We distinguish between a 'BimapCodec' and a non-'BimapCodec' just so that
  -- we don't introduce additional layers that we can already combine anyway.
  \case
    BimapCodec f' g' c -> BimapCodec (f' >=> f) (g' . g) c
    c -> BimapCodec f g c

-- | Vector codec
--
-- Build a codec for vectors of values from a codec for a single value.
--
--
-- === Example usage
--
-- >>> toJSONVia (vectorCodec codec) (Vector.fromList ['a','b'])
-- Array [String "a",String "b"]
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'ArrayOfCodec' without a name.
--
-- > vectorCodec = ArrayOfCodec Nothing
vectorCodec :: ValueCodec input output -> ValueCodec (Vector input) (Vector output)
vectorCodec = ArrayOfCodec Nothing

-- | List codec
--
-- Build a codec for lists of values from a codec for a single value.
--
--
-- === Example usage
--
-- >>> toJSONVia (listCodec codec) ['a','b']
-- Array [String "a",String "b"]
--
--
-- ==== API Note
--
-- This is the list version of 'vectorCodec'.
listCodec :: ValueCodec input output -> ValueCodec [input] [output]
listCodec = dimapCodec V.toList V.fromList . vectorCodec

-- | Build a codec for nonempty lists of values from a codec for a single value.
--
--
-- === Example usage
--
-- >>> toJSONVia (nonEmptyCodec codec) ('a' :| ['b'])
-- Array [String "a",String "b"]
--
--
-- ==== API Note
--
-- This is the non-empty list version of 'vectorCodec'.
nonEmptyCodec :: ValueCodec input output -> ValueCodec (NonEmpty input) (NonEmpty output)
nonEmptyCodec = bimapCodec parseNonEmptyList NE.toList . listCodec
  where
    parseNonEmptyList l = case NE.nonEmpty l of
      Nothing -> Left "Expected a nonempty list, but got an empty list."
      Just ne -> Right ne

-- | Single or list codec
--
-- This codec behaves like 'listCodec', except the values may also be
-- simplified as a single value.
--
-- During parsing, a single element may be parsed as the list of just that element.
-- During rendering, a list with only one element will be rendered as just that element.
--
--
-- === Example usage
--
-- >>> let c = singleOrListCodec codec :: JSONCodec [Int]
-- >>> toJSONVia c [5]
-- Number 5.0
-- >>> toJSONVia c [5,6]
-- Array [Number 5.0,Number 6.0]
-- >>> JSON.parseMaybe (parseJSONVia c) (Number 5) :: Maybe [Int]
-- Just [5]
-- >>> JSON.parseMaybe (parseJSONVia c) (Array [Number 5, Number 6]) :: Maybe [Int]
-- Just [5,6]
--
--
-- === WARNING
--
-- If you use nested lists, for example when the given value codec is also a
-- 'listCodec', you may get in trouble with ambiguities during parsing.
singleOrListCodec :: ValueCodec input output -> ValueCodec [input] [output]
singleOrListCodec c = dimapCodec f g $ eitherCodec c $ listCodec c
  where
    f = \case
      Left v -> [v]
      Right vs -> vs
    g = \case
      [v] -> Left v
      vs -> Right vs

-- | Single or nonempty list codec
--
-- This codec behaves like 'nonEmptyCodec', except the values may also be
-- simplified as a single value.
--
-- During parsing, a single element may be parsed as the list of just that element.
-- During rendering, a list with only one element will be rendered as just that element.
--
--
-- === Example usage
--
-- >>> let c = singleOrNonEmptyCodec codec :: JSONCodec (NonEmpty Int)
-- >>> toJSONVia c (5 :| [])
-- Number 5.0
-- >>> toJSONVia c (5 :| [6])
-- Array [Number 5.0,Number 6.0]
-- >>> JSON.parseMaybe (parseJSONVia c) (Number 5) :: Maybe (NonEmpty Int)
-- Just (5 :| [])
-- >>> JSON.parseMaybe (parseJSONVia c) (Array [Number 5, Number 6]) :: Maybe (NonEmpty Int)
-- Just (5 :| [6])
--
--
-- === WARNING
--
-- If you use nested lists, for example when the given value codec is also a
-- 'nonEmptyCodec', you may get in trouble with ambiguities during parsing.
--
-- ==== API Note
--
-- This is a nonempty version of 'singleOrListCodec'.
singleOrNonEmptyCodec :: ValueCodec input output -> ValueCodec (NonEmpty input) (NonEmpty output)
singleOrNonEmptyCodec c = dimapCodec f g $ eitherCodec c $ nonEmptyCodec c
  where
    f = \case
      Left v -> v :| []
      Right vs -> vs
    g = \case
      v :| [] -> Left v
      vs -> Right vs

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

-- | Like 'optionalFieldWithOmittedDefaultWith', but the value may also be
-- @null@ and that will be interpreted as the default value.
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

-- | Like 'optionalFieldWithOmittedDefaultWith'', but the value may also be
-- @null@ and that will be interpreted as the default value.
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

-- | Encode a 'HashMap', and decode any 'HashMap'.
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'HashMapCodec'.
--
-- > hashMapCodec = HashMapCodec
hashMapCodec ::
  (Eq k, Hashable k, FromJSONKey k, ToJSONKey k) =>
  JSONCodec v ->
  JSONCodec (HashMap k v)
hashMapCodec = HashMapCodec

-- | Encode a 'Map', and decode any 'Map'.
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'MapCodec'.
--
-- > mapCodec = MapCodec
mapCodec ::
  (Ord k, FromJSONKey k, ToJSONKey k) =>
  JSONCodec v ->
  JSONCodec (Map k v)
mapCodec = MapCodec

#if MIN_VERSION_aeson(2,0,0)
-- | Encode a 'KeyMap', and decode any 'KeyMap'.
--
-- This chooses 'hashMapCodec' or 'mapCodec' based on @ordered-keymap@ flag in aeson.
keyMapCodec ::
    -- |
    JSONCodec v ->
    -- |
    JSONCodec (KeyMap v)
keyMapCodec = case KM.coercionToMap of
  -- Can coerce to Map, use
  Just _ -> dimapCodec KM.fromMap KM.toMap . mapCodec
  -- Cannot coerce to Map, use HashMap instead.
  Nothing -> dimapCodec KM.fromHashMap KM.toHashMap . hashMapCodec
#endif

-- | Codec for a 'JSON.Value'
--
-- This is essentially your escape-hatch for when you would normally need a monad instance for 'Codec'.
-- You can build monad parsing by using 'valueCodec' together with 'bimapCodec' and supplying your own parsing function.
--
-- Note that this _does_ mean that the documentation will just say that you are parsing and rendering a value, so you may want to document the extra parsing further using '<?>'.
--
-- ==== API Note
--
-- This is a forward-compatible version of 'ValueCodec'.
--
-- > valueCodec = ValueCodec
valueCodec :: JSONCodec JSON.Value
valueCodec = ValueCodec

-- | Codec for @null@
--
--
-- === Example usage
--
-- >>> toJSONVia nullCodec ()
-- Null
-- >>> JSON.parseMaybe (parseJSONVia nullCodec) Null
-- Just ()
-- >>> JSON.parseMaybe (parseJSONVia nullCodec) (Number 5)
-- Nothing
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'NullCodec'.
--
-- > nullCodec = NullCodec
nullCodec :: JSONCodec ()
nullCodec = NullCodec

-- | Codec for boolean values
--
--
-- === Example usage
--
-- >>> toJSONVia boolCodec True
-- Bool True
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'BoolCodec' without a name.
--
-- > boolCodec = BoolCodec Nothing
boolCodec :: JSONCodec Bool
boolCodec = BoolCodec Nothing

-- | Codec for text values
--
--
-- === Example usage
--
-- >>> toJSONVia textCodec "hello"
-- String "hello"
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'StringCodec' without a name.
--
-- > textCodec = StringCodec Nothing
textCodec :: JSONCodec Text
textCodec = StringCodec Nothing

-- | Codec for 'String' values
--
--
-- === Example usage
--
-- >>> toJSONVia stringCodec "hello"
-- String "hello"
--
--
-- === WARNING
--
-- This codec uses 'T.unpack' and 'T.pack' to dimap a 'textCodec', so it __does not roundtrip__.
--
-- >>> toJSONVia stringCodec "\55296"
-- String "\65533"
--
--
-- ==== API Note
--
-- This is a 'String' version of 'textCodec'.
stringCodec :: JSONCodec String
stringCodec = dimapCodec T.unpack T.pack textCodec

-- | Codec for 'Scientific' values
--
--
-- === Example usage
--
-- >>> toJSONVia scientificCodec 5
-- Number 5.0
-- >>> JSON.parseMaybe (parseJSONVia scientificCodec) (Number 3)
-- Just 3.0
--
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
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'NumberCodec' without a name.
--
-- > scientificCodec = NumberCodec Nothing Nothing
scientificCodec :: JSONCodec Scientific
scientificCodec = NumberCodec Nothing Nothing

-- | Codec for 'Scientific' values with bounds
--
--
-- === Example usage
--
-- >>> let c = scientificWithBoundsCodec NumberBounds {numberBoundsLower = 2, numberBoundsUpper = 4}
-- >>> toJSONVia c 3
-- Number 3.0
-- >>> toJSONVia c 5
-- Number 5.0
-- >>> JSON.parseMaybe (parseJSONVia c) (Number 3)
-- Just 3.0
-- >>> JSON.parseMaybe (parseJSONVia c) (Number 5)
-- Nothing
--
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
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'NumberCodec' without a name.
--
-- > scientificWithBoundsCodec bounds = NumberCodec Nothing (Just bounds)
scientificWithBoundsCodec :: NumberBounds -> JSONCodec Scientific
scientificWithBoundsCodec bounds = NumberCodec Nothing (Just bounds)

-- | An object codec with a given name
--
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
--
--
-- ==== API Note
--
-- This is a forward-compatible version 'ObjectOfCodec' with a name.
--
-- > object name = ObjectOfCodec (Just name)
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
  bimapCodec go fromIntegral $ scientificWithBoundsCodec (boundedIntegralNumberBounds @i)
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

-- | A choice codec, but unlike 'eitherCodec', it's for the same output type instead of different ones.
--
-- While parsing, this codec will first try the left codec, then the right if that fails.
--
-- While rendering, the provided function is used to decide which codec to use for rendering.
--
-- Note: The reason this is less primitive than the 'eitherCodec' is that 'Either' makes it clear which codec you want to use for rendering.
-- In this case, we need to provide our own function for choosing which codec we want to use for rendering.
--
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
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'matchChoiceCodecAs PossiblyJointUnion':
--
-- > disjointMatchChoiceCodec = matchChoiceCodecAs PossiblyJointUnion
matchChoiceCodec ::
  -- | First codec
  Codec context input output ->
  -- | Second codec
  Codec context input' output ->
  -- | Rendering chooser
  (newInput -> Either input input') ->
  Codec context newInput output
matchChoiceCodec = matchChoiceCodecAs PossiblyJointUnion

-- | Disjoint version of 'matchChoiceCodec'
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'matchChoiceCodecAs DisjointUnion':
--
-- > disjointMatchChoiceCodec = matchChoiceCodecAs DisjointUnion
disjointMatchChoiceCodec ::
  -- | First codec
  Codec context input output ->
  -- | Second codec
  Codec context input' output ->
  -- | Rendering chooser
  (newInput -> Either input input') ->
  Codec context newInput output
disjointMatchChoiceCodec = matchChoiceCodecAs DisjointUnion

-- | An even more general version of 'matchChoiceCodec' and 'disjointMatchChoiceCodec'.
matchChoiceCodecAs ::
  -- | Is the union DisjointUnion or PossiblyJointUnion
  Union ->
  -- | First codec
  Codec context input output ->
  -- | Second codec
  Codec context input' output ->
  -- | Rendering chooser
  (newInput -> Either input input') ->
  Codec context newInput output
matchChoiceCodecAs union c1 c2 renderingChooser =
  dimapCodec (either id id) renderingChooser $
    EitherCodec union c1 c2

-- | A choice codec for a list of options, each with their own rendering matcher.
--
-- During parsing, each of the codecs are tried from first to last until one succeeds.
--
-- During rendering, each matching function is tried until either one succeeds and the corresponding codec is used, or none succeed and the fallback codec is used.
--
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
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'matchChoicesCodecAs DisjointUnion'.
--
-- > disjointMatchChoiceCodec = matchChoicesCodecAs DisjointUnion
matchChoicesCodec ::
  -- | Codecs, each with their own rendering matcher
  [(input -> Maybe input, Codec context input output)] ->
  -- | Fallback codec, in case none of the matchers in the list match
  Codec context input output ->
  Codec context input output
matchChoicesCodec = matchChoicesCodecAs PossiblyJointUnion

-- | Disjoint version of 'matchChoicesCodec'
--
--
-- ==== API Note
--
-- This is a forward-compatible version of 'matchChoicesCodecAs DisjointUnion'.
--
-- > disjointMatchChoiceCodec = matchChoicesCodecAs DisjointUnion
disjointMatchChoicesCodec ::
  -- | Codecs, each with their own rendering matcher
  [(input -> Maybe input, Codec context input output)] ->
  -- | Fallback codec, in case none of the matchers in the list match
  Codec context input output ->
  Codec context input output
disjointMatchChoicesCodec = matchChoicesCodecAs DisjointUnion

-- | An even more general version of 'matchChoicesCodec' and 'disjointMatchChoicesCodec'
matchChoicesCodecAs ::
  Union ->
  -- | Codecs, each with their own rendering matcher
  [(input -> Maybe input, Codec context input output)] ->
  -- | Fallback codec, in case none of the matchers in the list match
  Codec context input output ->
  Codec context input output
matchChoicesCodecAs union l fallback = go l
  where
    go = \case
      [] -> fallback
      ((m, c) : rest) -> matchChoiceCodecAs union c (go rest) $ \i -> case m i of
        Just j -> Left j
        Nothing -> Right i

-- | Use one codec for the default way of parsing and rendering, but then also
-- use a list of other codecs for potentially different parsing.
--
-- You can use this for keeping old ways of parsing intact while already rendering in the new way.
--
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
  Codec context input' output ->
  Codec context input output
parseAlternative c cAlt = matchChoiceCodec c cAlt Left

-- | A codec for an enum that can be written each with their own codec.
--
--
-- === WARNING
--
-- If you don't provide a string for one of the type's constructors, the last codec in the list will be used instead.
enumCodec ::
  forall enum context.
  Eq enum =>
  NonEmpty (enum, Codec context enum enum) ->
  Codec context enum enum
enumCodec = go
  where
    go :: NonEmpty (enum, Codec context enum enum) -> Codec context enum enum
    go ((e, c) :| rest) = case NE.nonEmpty rest of
      Nothing -> c
      Just ne -> disjointMatchChoiceCodec c (go ne) $ \i ->
        if e == i
          then Left e
          else Right i

-- | A codec for an enum that can be written as constant string values
--
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
  NonEmpty (constant, Text) ->
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
--
-- ==== API Note
--
-- This is a forward-compatible version of 'ReferenceCodec'.
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
