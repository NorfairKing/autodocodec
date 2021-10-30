{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Autodocodec.Class where

import Autodocodec.Codec
import qualified Data.Aeson as JSON
import Data.Int
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V
import Data.Word

class HasCodec a where
  -- | A codec for a single value
  --
  -- See the sections on helper functions for implementing this for plenty of examples.
  codec :: JSONCodec a

  -- | A codec for a list of values
  --
  -- This is really only useful for cases like 'Char' and 'String'
  listCodec :: JSONCodec [a]
  listCodec = dimapCodec V.toList V.fromList $ ArrayCodec Nothing codec

  {-# MINIMAL codec #-}

instance HasCodec Bool where
  codec = boolCodec

instance HasCodec Ordering where
  codec = shownBoundedEnumCodec

instance HasCodec Char where
  codec =
    let parseChar = \case
          [] -> Left "Expected exactly 1 character, but got none."
          [c] -> Right c
          _ -> Left "Expected exactly 1 character, but got more."
     in MapCodec parseChar (: []) stringCodec
  listCodec = stringCodec

instance HasCodec Text where
  codec = textCodec

instance HasCodec LT.Text where
  codec = dimapCodec LT.fromStrict LT.toStrict textCodec

instance HasCodec Scientific where
  codec = scientificCodec

instance HasCodec Int where
  codec = boundedIntegerCodec <?> "Int"

instance HasCodec Int8 where
  codec = boundedIntegerCodec <?> "Int8"

instance HasCodec Int16 where
  codec = boundedIntegerCodec <?> "Int16"

instance HasCodec Int32 where
  codec = boundedIntegerCodec <?> "Int32"

instance HasCodec Int64 where
  codec = boundedIntegerCodec <?> "Int64"

instance HasCodec Word where
  codec = boundedIntegerCodec <?> "Word"

instance HasCodec Word8 where
  codec = boundedIntegerCodec <?> "Word8"

instance HasCodec Word16 where
  codec = boundedIntegerCodec <?> "Word16"

instance HasCodec Word32 where
  codec = boundedIntegerCodec <?> "Word32"

instance HasCodec Word64 where
  codec = boundedIntegerCodec <?> "Word64"

instance HasCodec JSON.Value where
  codec = ValueCodec

instance HasCodec a => HasCodec (Maybe a) where
  codec = maybeCodec codec

instance (HasCodec l, HasCodec r) => HasCodec (Either l r) where
  codec =
    eitherCodec
      (ObjectCodec Nothing (requiredField' "Left"))
      (ObjectCodec Nothing (requiredField' "Right"))

instance HasCodec a => HasCodec [a] where
  codec = listCodec

-- | A required field
--
-- During decoding, the field must be in the object.
--
-- During encoding, the field will always be in the object.
--
-- See 'requiredFieldWith'
requiredField ::
  HasCodec output =>
  -- | Key
  Text ->
  -- | Documentation
  Text ->
  -- |
  ObjectCodec output output
requiredField key = requiredFieldWith key codec

-- | Like 'requiredField', but without documentation
requiredField' ::
  HasCodec output =>
  -- | Key
  Text ->
  -- |
  ObjectCodec output output
requiredField' key = requiredFieldWith' key codec

-- | An optional field
--
-- During decoding, the field may be in the object. 'Nothing' will be parsed otherwise.
--
-- During encoding, the field will be in the object if it is not 'Nothing', and omitted otherwise.
--
-- See 'optionalFieldWith'
optionalField ::
  HasCodec output =>
  -- | Key
  Text ->
  -- | Documentation
  Text ->
  -- |
  ObjectCodec (Maybe output) (Maybe output)
optionalField key = optionalFieldWith key codec

-- | Like 'optionalField', but without documentation
optionalField' ::
  HasCodec output =>
  -- | Key
  Text ->
  -- |
  ObjectCodec (Maybe output) (Maybe output)
optionalField' key = optionalFieldWith' key codec

-- | An optional field with a default value
--
-- During decoding, the field may be in the object. The default value will be parsed otherwise.
--
-- During encoding, the field will be in the object. The default value is ignored.
--
-- The shown version of the default value will appear in the documentation.
optionalFieldWithDefault ::
  (Show output, HasCodec output) =>
  -- | Key
  Text ->
  -- | Default value
  output ->
  -- | Documentation
  Text ->
  -- |
  ObjectCodec output output
optionalFieldWithDefault key defaultValue doc = optionalFieldWithDefaultWith key codec defaultValue doc

-- | Like 'optionalFieldWithDefault', but without documentation
optionalFieldWithDefault' ::
  (Show output, HasCodec output) =>
  -- | Key
  Text ->
  -- | Default value
  output ->
  -- |
  ObjectCodec output output
optionalFieldWithDefault' key defaultValue = optionalFieldWithDefaultWith' key codec defaultValue

-- | An optional, or null, field
--
-- During decoding, the field may be in the object. 'Nothing' will be parsed if it is not.
-- If the field is @null@, then it will be parsed as 'Nothing' as well.
--
-- During encoding, the field will be in the object if it is not 'Nothing', and omitted otherwise.
optionalFieldOrNull ::
  forall output.
  HasCodec output =>
  -- | Key
  Text ->
  -- | Documentation
  Text ->
  ObjectCodec (Maybe output) (Maybe output)
optionalFieldOrNull key doc =
  dimapCodec f g $ OptionalKeyCodec key (maybeCodec codec) (Just doc)
  where
    f :: Maybe (Maybe output) -> Maybe output
    f = \case
      Nothing -> Nothing
      Just Nothing -> Nothing
      Just (Just a) -> Just a
    g :: Maybe output -> Maybe (Maybe output)
    g = \case
      Nothing -> Nothing
      Just a -> Just (Just a)

-- | Like 'optionalFieldOrNull', but without documentation
optionalFieldOrNull' ::
  forall output.
  HasCodec output =>
  -- | Key
  Text ->
  ObjectCodec (Maybe output) (Maybe output)
optionalFieldOrNull' key =
  dimapCodec f g $ OptionalKeyCodec key (maybeCodec codec) Nothing
  where
    f :: Maybe (Maybe output) -> Maybe output
    f = \case
      Nothing -> Nothing
      Just Nothing -> Nothing
      Just (Just a) -> Just a
    g :: Maybe output -> Maybe (Maybe output)
    g = \case
      Nothing -> Nothing
      Just a -> Just (Just a)
