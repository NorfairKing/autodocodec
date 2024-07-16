{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
-- Because Eq is a superclass of Hashable in newer versions.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Autodocodec.Class where

import Autodocodec.Codec
import Data.Aeson (FromJSONKey, ToJSONKey)
import qualified Data.Aeson as JSON
import Numeric.Natural
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap (KeyMap)
#endif
import Data.Functor.Const (Const (Const))
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Int
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Monoid as Monoid
import Data.Scientific
import Data.Semigroup (Dual (Dual))
import qualified Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Vector (Vector)
import Data.Void
import Data.Word

-- | A class for values which have a canonical codec.
--
-- There are no formal laws for this class.
-- If you really want a law, it should be "Whomever uses the 'codec' from your instance should not be surprised."
class HasCodec value where
  -- | A codec for a single value
  --
  -- See the sections on helper functions for implementing this for plenty of examples.
  codec :: JSONCodec value

  -- | A codec for a list of values
  --
  -- This is really only useful for cases like 'Char' and 'String'.
  -- We didn't call it 'listCodec' so we could use that name for making a codec for a list of values from a single codec instead.
  listCodecForStringCompatibility :: JSONCodec [value]
  listCodecForStringCompatibility = listCodec codec

  {-# MINIMAL codec #-}

instance HasCodec Void where
  codec = bimapCodec (\_ -> Left "Cannot decode a Void.") absurd valueCodec

instance HasCodec Bool where
  codec = boolCodec

instance HasCodec Ordering where
  codec = shownBoundedEnumCodec

instance HasCodec Char where
  codec =
    let parseChar = \case
          [] -> Left "Expected exactly 1 character, but got none."
          [c] -> Right c
          s -> Left $ "Expected exactly 1 character, but got more:" <> s
     in bimapCodec parseChar (: []) stringCodec
  listCodecForStringCompatibility = stringCodec

instance HasCodec Text where
  codec = textCodec

instance HasCodec LT.Text where
  codec = dimapCodec LT.fromStrict LT.toStrict textCodec

instance HasCodec Scientific where
  codec = scientificCodec

instance HasCodec Int where
  codec = boundedIntegralCodec

instance HasCodec Int8 where
  codec = boundedIntegralCodec

instance HasCodec Int16 where
  codec = boundedIntegralCodec

instance HasCodec Int32 where
  codec = boundedIntegralCodec

instance HasCodec Int64 where
  codec = boundedIntegralCodec

-- | This instance uses the "safe" 'integerCodec'.
instance HasCodec Integer where
  codec = integerCodec

instance HasCodec Word where
  codec = boundedIntegralCodec

instance HasCodec Word8 where
  codec = boundedIntegralCodec

instance HasCodec Word16 where
  codec = boundedIntegralCodec

instance HasCodec Word32 where
  codec = boundedIntegralCodec

instance HasCodec Word64 where
  codec = boundedIntegralCodec

-- | This instance uses the "safe" 'naturalCodec'.
instance HasCodec Natural where
  codec = naturalCodec

instance HasCodec JSON.Value where
  codec = valueCodec

deriving newtype instance (HasCodec a) => HasCodec (Identity a)

deriving newtype instance (HasCodec a) => HasCodec (Dual a)

deriving newtype instance (HasCodec a) => HasCodec (Semigroup.First a)

deriving newtype instance (HasCodec a) => HasCodec (Semigroup.Last a)

deriving newtype instance (HasCodec a) => HasCodec (Monoid.First a)

deriving newtype instance (HasCodec a) => HasCodec (Monoid.Last a)

deriving newtype instance (HasCodec a) => HasCodec (Const a b)

instance (HasCodec a) => HasCodec (Maybe a) where
  codec = maybeCodec codec

instance (HasCodec l, HasCodec r) => HasCodec (Either l r) where
  codec =
    disjointEitherCodec
      (ObjectOfCodec Nothing (requiredField' "Left"))
      (ObjectOfCodec Nothing (requiredField' "Right"))

instance (HasCodec a) => HasCodec (Vector a) where
  codec = vectorCodec codec

instance (HasCodec a) => HasCodec [a] where
  codec = listCodecForStringCompatibility

instance (HasCodec a) => HasCodec (NonEmpty a) where
  codec = nonEmptyCodec codec

instance (Ord a, HasCodec a) => HasCodec (Set a) where
  codec = dimapCodec S.fromList S.toList codec

instance (Ord k, FromJSONKey k, ToJSONKey k, HasCodec v) => HasCodec (Map k v) where
  codec = mapCodec codec

instance (Eq k, Hashable k, FromJSONKey k, ToJSONKey k, HasCodec v) => HasCodec (HashMap k v) where
  codec = hashMapCodec codec

#if MIN_VERSION_aeson(2,0,0)
instance HasCodec v => HasCodec (KeyMap v) where
  codec = keyMapCodec codec
#endif

-- TODO make these instances better once aeson exposes its @Data.Aeson.Parser.Time@ or @Data.Attoparsec.Time@ modules.
instance HasCodec Day where
  codec = codecViaAeson "Day"

instance HasCodec LocalTime where
  codec = codecViaAeson "LocalTime"

instance HasCodec UTCTime where
  codec = codecViaAeson "LocalTime"

instance HasCodec TimeOfDay where
  codec = codecViaAeson "TimeOfDay"

instance HasCodec ZonedTime where
  codec = codecViaAeson "ZonedTime"

instance HasCodec NominalDiffTime where
  codec = dimapCodec realToFrac realToFrac (codec :: JSONCodec Scientific)

instance HasCodec DiffTime where
  codec = dimapCodec realToFrac realToFrac (codec :: JSONCodec Scientific)

-- | A class for values which have a canonical object codec.
--
-- There are no formal laws for this class.
-- If you really want a law, it should be "Whomever uses the 'codec' from your instance should not be surprised."
class HasObjectCodec object where
  -- | A object codec for the value
  --
  -- See the sections on helper functions for implementing this for plenty of examples.
  objectCodec :: JSONObjectCodec object

-- | A required field
--
-- During decoding, the field must be in the object.
--
-- During encoding, the field will always be in the object.
--
-- See 'requiredFieldWith'
requiredField ::
  (HasCodec output) =>
  -- | Key
  Text ->
  -- | Documentation
  Text ->
  ObjectCodec output output
requiredField key = requiredFieldWith key codec

-- | Like 'requiredField', but without documentation
requiredField' ::
  (HasCodec output) =>
  -- | Key
  Text ->
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
  (HasCodec output) =>
  -- | Key
  Text ->
  -- | Documentation
  Text ->
  ObjectCodec (Maybe output) (Maybe output)
optionalField key = optionalFieldWith key codec

-- | Like 'optionalField', but without documentation
optionalField' ::
  (HasCodec output) =>
  -- | Key
  Text ->
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
  (HasCodec output) =>
  -- | Key
  Text ->
  -- | Default value
  output ->
  -- | Documentation
  Text ->
  ObjectCodec output output
optionalFieldWithDefault key defaultValue doc = optionalFieldWithDefaultWith key codec defaultValue doc

-- | Like 'optionalFieldWithDefault', but without documentation
optionalFieldWithDefault' ::
  (HasCodec output) =>
  -- | Key
  Text ->
  -- | Default value
  output ->
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
  (HasCodec output) =>
  -- | Key
  Text ->
  -- | Documentation
  Text ->
  ObjectCodec (Maybe output) (Maybe output)
optionalFieldOrNull key doc = orNullHelper $ optionalKeyCodec key (maybeCodec codec) (Just doc)

-- | Like 'optionalFieldOrNull', but without documentation
optionalFieldOrNull' ::
  forall output.
  (HasCodec output) =>
  -- | Key
  Text ->
  ObjectCodec (Maybe output) (Maybe output)
optionalFieldOrNull' key = orNullHelper $ optionalKeyCodec key (maybeCodec codec) Nothing

optionalFieldWithOmittedDefault ::
  (Eq output, HasCodec output) =>
  -- | Key
  Text ->
  -- | Default value
  output ->
  -- | Documentation
  Text ->
  ObjectCodec output output
optionalFieldWithOmittedDefault key defaultValue doc = optionalFieldWithOmittedDefaultWith key codec defaultValue doc

optionalFieldWithOmittedDefault' ::
  (Eq output, HasCodec output) =>
  -- | Key
  Text ->
  -- | Default value
  output ->
  ObjectCodec output output
optionalFieldWithOmittedDefault' key defaultValue = optionalFieldWithOmittedDefaultWith' key codec defaultValue

optionalFieldOrNullWithOmittedDefault ::
  (Eq output, HasCodec output) =>
  -- | Key
  Text ->
  -- | Default value
  output ->
  -- | Documentation
  Text ->
  ObjectCodec output output
optionalFieldOrNullWithOmittedDefault key defaultValue doc = optionalFieldOrNullWithOmittedDefaultWith key codec defaultValue doc

optionalFieldOrNullWithOmittedDefault' ::
  (Eq output, HasCodec output) =>
  -- | Key
  Text ->
  -- | Default value
  output ->
  ObjectCodec output output
optionalFieldOrNullWithOmittedDefault' key defaultValue = optionalFieldOrNullWithOmittedDefaultWith' key codec defaultValue
