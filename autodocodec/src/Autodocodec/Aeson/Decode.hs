{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Autodocodec.Aeson.Decode
  ( -- * Decoding JSON Values
    parseJSONViaCodec,
    parseJSONVia,

    -- ** Decoding JSON Objects
    parseJSONObjectViaCodec,
    parseJSONObjectVia,

    -- ** Internal
    parseJSONContextVia,

    -- * Decoding "Trees That Grow" extension nodes
    ParseExt (..),
    parseJSONViaExt,
    parseJSONObjectViaExt,
    parseJSONContextViaExt,
  )
where

import qualified Autodocodec.Aeson.Compat as Compat
import Autodocodec.Class
import Autodocodec.Codec
import Control.Monad
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Implement 'JSON.parseJSON' via a type's codec.
parseJSONViaCodec :: (HasCodec a) => JSON.Value -> JSON.Parser a
parseJSONViaCodec = parseJSONVia codec

-- | Implement 'JSON.parseJSON' via a given codec.
parseJSONVia :: ValueCodec void a -> JSON.Value -> JSON.Parser a
parseJSONVia = parseJSONContextVia

parseJSONObjectViaCodec :: (HasObjectCodec a) => JSON.Object -> JSON.Parser a
parseJSONObjectViaCodec = parseJSONObjectVia objectCodec

parseJSONObjectVia :: ObjectCodec void a -> JSON.Object -> JSON.Parser a
parseJSONObjectVia = parseJSONContextVia

-- | Parse via a general codec.
--
-- You probably won't need this. See 'eitherDecodeViaCodec', 'parseJSONViaCodec' and 'parseJSONVia' instead.
parseJSONContextVia :: Codec Vanilla context void a -> context -> JSON.Parser a
parseJSONContextVia = parseJSONContextViaExt vanillaParseExt

-- Everything below is for parsing 'Codec's at phases other than 'Vanilla',
-- i.e. codecs that may contain 'XCodec' "Trees That Grow" extension nodes.

-- | How to parse the 'XCodec' extension nodes of a given @phase@.
--
-- The handler receives the extension payload and the 'JSON.Value' at the node
-- and produces a parser for the value (recovered at 'XVal').
--
-- At the 'Vanilla' phase there are no extension nodes, so 'vanillaParseExt'
-- provides a handler that can never be called.
newtype ParseExt phase = ParseExt
  { runParseExt :: XXCodec phase -> JSON.Value -> JSON.Parser (XVal phase)
  }

-- | The 'ParseExt' for the 'Vanilla' phase; its handler is unreachable.
vanillaParseExt :: ParseExt Vanilla
vanillaParseExt = ParseExt (\x _ -> noExtCon x)

-- | Like 'parseJSONVia', but for a 'Codec' at any @phase@, given a handler for
-- its extension nodes.
parseJSONViaExt :: ParseExt phase -> Codec phase JSON.Value void a -> JSON.Value -> JSON.Parser a
parseJSONViaExt = parseJSONContextViaExt

-- | Like 'parseJSONObjectVia', but for a 'Codec' at any @phase@, given a
-- handler for its extension nodes.
parseJSONObjectViaExt :: ParseExt phase -> Codec phase JSON.Object void a -> JSON.Object -> JSON.Parser a
parseJSONObjectViaExt = parseJSONContextViaExt

-- | Like 'parseJSONContextVia', but for a 'Codec' at any @phase@, given a
-- handler for its extension nodes.
parseJSONContextViaExt :: forall phase context void a. ParseExt phase -> Codec phase context void a -> context -> JSON.Parser a
parseJSONContextViaExt ext codec_ context_ =
  modifyFailure (\s -> if '\n' `elem` s then "\n" ++ s else s) $
    go context_ codec_
  where
    -- We use type-annotations here for readability of type information that is
    -- gathered to case-matching on GADTs, they aren't strictly necessary.
    go :: forall context' void' a'. context' -> Codec phase context' void' a' -> JSON.Parser a'
    go value = \case
      NullCodec _ -> case (value :: JSON.Value) of
        Null -> coerce (pure () :: JSON.Parser ())
        _ -> typeMismatch "Null" value
      BoolCodec _ mname -> case mname of
        Nothing -> coerce (parseJSON value :: JSON.Parser Bool)
        Just name -> coerce $ withBool (T.unpack name) pure value
      StringCodec _ mname -> case mname of
        Nothing -> coerce (parseJSON value :: JSON.Parser Text)
        Just name -> coerce $ withText (T.unpack name) pure value
      IntegerCodec _ mname bounds ->
        coerce $
          ( \f -> do
              let safetyBounds =
                    Bounds
                      { boundsLower = Just $ scientific (-1) 1024,
                        boundsUpper = Just $ scientific 1 1024
                      }
                  checkSafetyBounds s =
                    case checkBounds safetyBounds s of
                      Left err -> fail err
                      Right i' -> pure i'
              s <- case mname of
                Nothing -> parseJSON value >>= checkSafetyBounds
                Just name -> withScientific (T.unpack name) checkSafetyBounds value
              case Scientific.floatingOrInteger s :: Either Double Integer of
                Left _ -> fail $ "Number was not integer: " <> show s
                Right i -> f (i :: Integer)
          )
            ( \i -> case checkBounds bounds i of
                Left err -> fail err
                Right i' -> pure i'
            )
      NumberCodec _ mname bounds ->
        coerce $
          ( \f -> case mname of
              Nothing -> parseJSON value >>= f
              Just name -> withScientific (T.unpack name) f value
          )
            ( \s -> case checkBounds bounds s of
                Left err -> fail err
                Right s' -> pure s'
            )
      ArrayOfCodec _ mname c ->
        ( \f -> case mname of
            Nothing -> parseJSON value >>= f
            Just name -> withArray (T.unpack name) f value
        )
          ( \vector ->
              coerce $
                forM
                  (V.indexed (vector :: Vector JSON.Value))
                  ( \(ix, v) ->
                      go v c JSON.<?> Index ix
                  )
          )
      ObjectOfCodec _ mname c ->
        ( \f -> case mname of
            Nothing -> parseJSON value >>= f
            Just name -> withObject (T.unpack name) f value
        )
          (\object_ -> (`go` c) (object_ :: JSON.Object))
      HashMapCodec _ c -> coerce (Compat.liftParseJSON (`go` c) (goList c) value :: JSON.Parser (HashMap _ _))
      MapCodec _ c -> coerce (Compat.liftParseJSON (`go` c) (goList c) value :: JSON.Parser (Map _ _))
      ValueCodec _ -> pure $ coerce value
      EqCodec _ expected c -> do
        actual <- go value c
        if expected == actual
          then pure (coerce actual)
          else fail $ unwords ["Expected", show expected, "but got", show actual]
      BimapCodec _ f _ c -> do
        old <- go value c
        case f old of
          Left err -> fail err
          Right new -> pure new
      EitherCodec _ u c1 c2 ->
        let leftParser v = Left <$> go v c1
            rightParser v = Right <$> go v c2
         in coerce $ case u of
              PossiblyJointUnion ->
                case parseEither leftParser value of
                  Right l -> pure l
                  Left err -> prependFailure ("  Previous branch failure: " <> err <> "\n") (rightParser value)
              DisjointUnion ->
                case (parseEither leftParser value, parseEither rightParser value) of
                  (Right l, Left _) -> pure l
                  (Left _, Right r) -> pure r
                  (Right _, Right _) -> fail "Both branches of a disjoint union succeeded."
                  (Left lErr, Left rErr) ->
                    fail $
                      unlines
                        [ "Both branches of a disjoint union failed: ",
                          unwords ["Left:  ", lErr],
                          unwords ["Right: ", rErr]
                        ]
      DiscriminatedUnionCodec _ propertyName _ m -> do
        discriminatorValue <- (value :: JSON.Object) JSON..: Compat.toKey propertyName
        case HashMap.lookup discriminatorValue m of
          Nothing -> fail $ "Unexpected discriminator value: " <> show discriminatorValue
          Just (_, c) ->
            go value c
      CommentCodec _ _ c -> go value c
      ReferenceCodec _ _ c -> go value c
      RequiredKeyCodec _ k c _ -> do
        valueAtKey <- (value :: JSON.Object) JSON..: Compat.toKey k
        coerce $ go valueAtKey c JSON.<?> Key (Compat.toKey k)
      OptionalKeyCodec _ k c _ -> do
        let key = Compat.toKey k
            mValueAtKey = Compat.lookupKey key (value :: JSON.Object)
        coerce $ forM mValueAtKey $ \valueAtKey -> go (valueAtKey :: JSON.Value) c JSON.<?> Key key
      OptionalKeyWithDefaultCodec _ k c defaultValue _ -> do
        let key = Compat.toKey k
            mValueAtKey = Compat.lookupKey key (value :: JSON.Object)
        coerce $ case mValueAtKey of
          Nothing -> pure defaultValue
          Just valueAtKey -> go (valueAtKey :: JSON.Value) c JSON.<?> Key key
      OptionalKeyWithOmittedDefaultCodec _ k c defaultValue _ -> do
        let key = Compat.toKey k
            mValueAtKey = Compat.lookupKey key (value :: JSON.Object)
        coerce $ case mValueAtKey of
          Nothing -> pure defaultValue
          Just valueAtKey -> go (valueAtKey :: JSON.Value) c JSON.<?> Key key
      PureCodec _ a -> pure a
      ApCodec _ ocf oca -> go (value :: JSON.Object) ocf <*> go (value :: JSON.Object) oca
      XCodec meta -> coerce <$> runParseExt ext meta value
    -- A phase-polymorphic list parser, mirroring 'listCodec's parsing: parse an
    -- array and parse each element with the element codec. Inlined because the
    -- 'Vanilla'-pinned 'listCodec' would not typecheck at an arbitrary phase.
    goList :: forall elemInput elemOutput. Codec phase JSON.Value elemInput elemOutput -> JSON.Value -> JSON.Parser [elemOutput]
    goList c v = do
      vec <- parseJSON v :: JSON.Parser (Vector JSON.Value)
      V.toList <$> V.imapM (\ix el -> go el c JSON.<?> Index ix) vec
