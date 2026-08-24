{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Interpret an 'ObjectCodec' as a @application/x-www-form-urlencoded@ 'Form'.
--
-- A 'Form' is flat: a map from keys to lists of text values.
-- Consequently:
--
-- * A nested object or array is JSON-encoded into the text slot of its key.
--   That round-trips through this module but no other form parser will
--   understand it.
-- * A field is absent exactly when its key is absent; a 'Form' has no @null@
--   of its own. See 'EmptyValue' if you need @key=@ to mean absent as well,
--   which is a decoder setting because only an optional key can be absent.
-- * An optional field holding an empty list decodes as absent rather than as
--   the empty list, because a key with no values is not something a form can
--   carry. Use a required field, or a non-empty list, if you need to tell those
--   apart.
module Autodocodec.FormUrlEncoded where

import Autodocodec
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LB
import Data.Coerce (coerce)
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Web.FormUrlEncoded

toFormViaCodec :: forall a. (HasObjectCodec a) => a -> Form
toFormViaCodec = toFormVia (objectCodec @a)

toFormVia :: ObjectCodec a void -> a -> Form
toFormVia = flip go
  where
    go :: a -> ObjectCodec a void -> Form
    go a = \case
      BimapCodec _ to c -> go (to a) c
      EitherCodec _ c1 c2 -> case coerce a of
        Left a1 -> go a1 c1
        Right a2 -> go a2 c2
      DiscriminatedUnionCodec discriminator encoding _ ->
        let (discriminatorValue, c) = encoding a
         in unionForm
              (singletonForm discriminator [discriminatorValue])
              (go a c)
      RequiredKeyCodec key vc _ -> singletonForm key (goValue (coerce a) vc)
      OptionalKeyCodec key vc _ ->
        singletonForm key $ do
          a' <- maybeToList $ coerce a
          goValue a' vc
      OptionalKeyWithDefaultCodec key vc _ _ -> singletonForm key (goValue a vc)
      OptionalKeyWithOmittedDefaultCodec key vc defaultValue _ ->
        if coerce a == defaultValue
          then emptyForm
          else singletonForm key (goValue (coerce a) vc)
      PureCodec _ -> emptyForm
      ApCodec oc1 oc2 -> unionForm (go a oc1) (go a oc2)

    goValue :: a -> ValueCodec a void -> [Text]
    goValue a = \case
      BimapCodec _ to vc -> goValue (to a) vc
      EitherCodec _ c1 c2 -> case coerce a of
        Left a1 -> goValue a1 c1
        Right a2 -> goValue a2 c2
      CommentCodec _ vc -> goValue a vc
      ArrayOfCodec _ (vc :: ValueCodec input output) -> map (`goSingleValue` vc) (toList (coerce a :: Vector input))
      vc -> [goSingleValue a vc]

    goSingleValue :: a -> ValueCodec a void -> Text
    goSingleValue a = \case
      BimapCodec _ to vc -> goSingleValue (to a) vc
      EitherCodec _ c1 c2 -> case coerce a of
        Left a1 -> goSingleValue a1 c1
        Right a2 -> goSingleValue a2 c2
      CommentCodec _ vc -> goSingleValue a vc
      NullCodec -> "null"
      -- Lower case, to match @toUrlPiece \@Bool@ and the HTML form convention,
      -- so that the value survives http-api-data's own parser.
      BoolCodec _ ->
        case coerce a of
          True -> "true"
          False -> "false"
      StringCodec _ _ -> coerce a
      vc ->
        let value = toJSONVia vc a
         in case value of
              JSON.String t -> t
              _ -> TE.decodeUtf8 (LB.toStrict (JSON.encode value))

emptyForm :: Form
emptyForm = Form HashMap.empty

-- | A 'Form' with a single key, or 'emptyForm' if there are no values.
--
-- A key mapped to no values would encode to nothing anyway, so keeping it out
-- of the map means a 'Form' is 'Eq' to one built by any other route.
singletonForm :: Text -> [Text] -> Form
singletonForm key = \case
  [] -> emptyForm
  ts -> Form (HashMap.singleton key ts)

-- | Combine two 'Form's, keeping the values of both under a shared key.
--
-- Not '<>': 'Form' derives 'Semigroup' from 'HashMap', whose union is
-- left-biased and would silently drop the second form's values.
unionForm :: Form -> Form -> Form
unionForm (Form h1) (Form h2) = Form (HashMap.unionWith (++) h1 h2)

instance (HasObjectCodec a) => ToForm (Autodocodec a) where
  toForm = toFormViaCodec . unAutodocodec

-- | How to read a key that is present but whose every value is empty, as in
-- @key=@.
--
-- This only ever applies to optional keys. A required key always decodes its
-- value, empty or not, because there is no absence for it to mean.
data EmptyValue
  = -- | An empty value is a value, so @key=@ decodes as the empty string.
    --
    -- Lossless, and it agrees with 'lookupMaybe', which distinguishes a missing
    -- key from an empty one.
    EmptyValueIsValue
  | -- | An empty value means the optional key is absent.
    --
    -- A shell substitutes the empty string for an unset variable, so
    -- @curl --data-urlencode "key=$UNSET"@ sends @key=@ rather than nothing at
    -- all. Choosing this makes an optional field that is genuinely the empty
    -- string inexpressible.
    EmptyValueIsAbsent

data FormDecodeSettings = FormDecodeSettings
  { formDecodeSettingEmptyValue :: !EmptyValue
  }

defaultFormDecodeSettings :: FormDecodeSettings
defaultFormDecodeSettings =
  FormDecodeSettings
    { formDecodeSettingEmptyValue = EmptyValueIsValue
    }

fromFormViaCodec :: forall a. (HasObjectCodec a) => Form -> Either String a
fromFormViaCodec = fromFormViaCodecWith defaultFormDecodeSettings

fromFormViaCodecWith :: forall a. (HasObjectCodec a) => FormDecodeSettings -> Form -> Either String a
fromFormViaCodecWith settings = fromFormViaWith settings (objectCodec @a)

fromFormVia :: ObjectCodec void a -> Form -> Either String a
fromFormVia = fromFormViaWith defaultFormDecodeSettings

fromFormViaWith :: FormDecodeSettings -> ObjectCodec void a -> Form -> Either String a
fromFormViaWith FormDecodeSettings {..} = flip go
  where
    -- Name the key in whatever failed under it. This error reaches whoever
    -- posted the form, often as an HTTP response body, so "which key" is the
    -- first thing it has to say.
    inKey :: Text -> Either String b -> Either String b
    inKey key = first $ \err -> concat ["Failed to parse key ", show key, ": ", err]

    -- The values of an optional key, with 'formDecodeSettingEmptyValue'
    -- applied. A key whose values are not all empty is left alone, so that an
    -- empty element among non-empty ones is still an element.
    lookupOptional :: Text -> Form -> [Text]
    lookupOptional key form =
      let values = lookupAll key form
       in case formDecodeSettingEmptyValue of
            EmptyValueIsValue -> values
            EmptyValueIsAbsent
              | all Text.null values -> []
              | otherwise -> values

    go :: Form -> ObjectCodec void a -> Either String a
    go form = \case
      BimapCodec from _ c -> go form c >>= from
      EitherCodec u c1 c2 -> coerce $ case u of
        PossiblyJointUnion ->
          case go form c1 of
            Right l -> pure (Left l)
            Left err1 -> case go form c2 of
              Left err2 -> Left $ concat ["  Previous branch failure: ", err1, "\n", err2]
              Right r -> pure (Right r)
        DisjointUnion ->
          case (go form c1, go form c2) of
            (Left _, Right r) -> pure (Right r)
            (Right l, Left _) -> pure (Left l)
            (Right _, Right _) -> Left "Both branches of a disjoint union succeeded."
            (Left lErr, Left rErr) ->
              Left $
                unlines
                  [ "Both branches of a disjoint union failed: ",
                    unwords ["Left:  ", lErr],
                    unwords ["Right: ", rErr]
                  ]
      DiscriminatedUnionCodec discriminator _ m -> do
        discriminatorValue <- first Text.unpack $ lookupUnique discriminator form
        case HashMap.lookup discriminatorValue m of
          Nothing -> Left $ unwords ["Unexpected discriminator value:", show discriminatorValue]
          Just (_, c) -> go form c
      RequiredKeyCodec key vc _ -> inKey key $ coerce $ goValue (lookupAll key form) vc
      OptionalKeyCodec key vc _ -> inKey key $ coerce $ case lookupOptional key form of
        [] -> pure Nothing
        values -> Just <$> goValue values vc
      OptionalKeyWithDefaultCodec key vc defaultValue _ -> inKey key $ coerce $ case lookupOptional key form of
        [] -> pure defaultValue
        values -> goValue values vc
      OptionalKeyWithOmittedDefaultCodec key vc defaultValue _ -> inKey key $ coerce $ case lookupOptional key form of
        [] -> pure defaultValue
        values -> goValue values vc
      PureCodec v -> pure v
      ApCodec ocf oca -> go form ocf <*> go form oca

    goValue :: [Text] -> ValueCodec void a -> Either String a
    goValue ts = \case
      BimapCodec from _ c -> goValue ts c >>= from
      EitherCodec u c1 c2 -> coerce $ case u of
        PossiblyJointUnion ->
          case goValue ts c1 of
            Right l -> pure (Left l)
            Left err1 -> case goValue ts c2 of
              Left err2 -> Left $ concat ["  Previous branch failure: ", err1, "\n", err2]
              Right r -> pure (Right r)
        DisjointUnion ->
          case (goValue ts c1, goValue ts c2) of
            (Left _, Right r) -> pure (Right r)
            (Right l, Left _) -> pure (Left l)
            (Right _, Right _) -> Left "Both branches of a disjoint union succeeded."
            (Left lErr, Left rErr) ->
              Left $
                unlines
                  [ "Both branches of a disjoint union failed: ",
                    unwords ["Left:  ", lErr],
                    unwords ["Right: ", rErr]
                  ]
      ReferenceCodec _ vc -> goValue ts vc
      CommentCodec _ c -> goValue ts c
      ArrayOfCodec _ vc -> coerce $ V.fromList <$> mapM (`goSingleValue` vc) (toList ts)
      vc -> case ts of
        [t] -> goSingleValue t vc
        [] -> Left "expected exactly one value, found none."
        _ -> Left $ concat ["expected exactly one value, found ", show (length ts), "."]

    goSingleValue :: Text -> ValueCodec void a -> Either String a
    goSingleValue t = \case
      BimapCodec from _ c -> goSingleValue t c >>= from
      EitherCodec u c1 c2 -> coerce $ case u of
        PossiblyJointUnion ->
          case goSingleValue t c1 of
            Right l -> pure (Left l)
            Left err1 -> case goSingleValue t c2 of
              Left err2 -> Left $ concat ["  Previous branch failure: ", err1, "\n", err2]
              Right r -> pure (Right r)
        DisjointUnion ->
          case (goSingleValue t c1, goSingleValue t c2) of
            (Left _, Right r) -> pure (Right r)
            (Right l, Left _) -> pure (Left l)
            (Right _, Right _) -> Left "Both branches of a disjoint union succeeded."
            (Left lErr, Left rErr) ->
              Left $
                unlines
                  [ "Both branches of a disjoint union failed: ",
                    unwords ["Left:  ", lErr],
                    unwords ["Right: ", rErr]
                  ]
      CommentCodec _ c -> goSingleValue t c
      ReferenceCodec _ vc -> goSingleValue t vc
      NullCodec -> coerce $ case t of
        "null" -> Right ()
        _ -> Left $ unwords ["not 'null':", show t]
      BoolCodec _ -> coerce $ case t of
        "false" -> Right False
        "False" -> Right False
        "true" -> Right True
        "True" -> Right True
        _ -> Left $ unwords ["Unknown bool:", show t]
      StringCodec _ _ -> Right (coerce t)
      vc -> case JSON.parseEither (parseJSONVia vc) (JSON.String t) of
        Right a -> Right a
        Left _ -> do
          value <- JSON.eitherDecode (LB.fromStrict (TE.encodeUtf8 t))
          JSON.parseEither (parseJSONVia vc) value

instance (HasObjectCodec a) => FromForm (Autodocodec a) where
  fromForm = first Text.pack . fmap Autodocodec . fromFormViaCodec
