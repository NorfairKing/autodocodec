{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-orphans #-}

module Autodocodec.Aeson.Decode
  ( -- * Decoding JSON Values
    parseJSONViaCodec,
    parseJSONVia,

    -- ** Decoding JSON Objects
    parseJSONObjectViaCodec,
    parseJSONObjectVia,

    -- ** Internal
    parseJSONContextVia,
  )
where

import qualified Autodocodec.Aeson.Compat as Compat
import Autodocodec.Class
import Autodocodec.Codec
import Autodocodec.DerivingVia
import Control.Monad
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
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
parseJSONContextVia :: Codec context void a -> context -> JSON.Parser a
parseJSONContextVia codec_ context_ =
  modifyFailure (\s -> if '\n' `elem` s then "\n" ++ s else s) $
    go context_ codec_
  where
    -- We use type-annotations here for readability of type information that is
    -- gathered to case-matching on GADTs, they aren't strictly necessary.
    go :: context -> Codec context void a -> JSON.Parser a
    go value = \case
      NullCodec -> case (value :: JSON.Value) of
        Null -> pure ()
        _ -> typeMismatch "Null" value
      BoolCodec mname -> case mname of
        Nothing -> parseJSON value
        Just name -> withBool (T.unpack name) pure value
      StringCodec mname -> case mname of
        Nothing -> parseJSON value
        Just name -> withText (T.unpack name) pure value
      NumberCodec mname mBounds ->
        ( \f -> case mname of
            Nothing -> parseJSON value >>= f
            Just name -> withScientific (T.unpack name) f value
        )
          ( \s -> case maybe Right checkNumberBounds mBounds s of
              Left err -> fail err
              Right s' -> pure s'
          )
      ArrayOfCodec mname c ->
        ( \f -> case mname of
            Nothing -> parseJSON value >>= f
            Just name -> withArray (T.unpack name) f value
        )
          ( \vector ->
              forM
                (V.indexed (vector :: Vector JSON.Value))
                ( \(ix, v) ->
                    go v c JSON.<?> Index ix
                )
          )
      ObjectOfCodec mname c ->
        ( \f -> case mname of
            Nothing -> parseJSON value >>= f
            Just name -> withObject (T.unpack name) f value
        )
          (\object_ -> (`go` c) (object_ :: JSON.Object))
      HashMapCodec c -> Compat.liftParseJSON (`go` c) (`go` listCodec c) value :: JSON.Parser (HashMap _ _)
      MapCodec c -> Compat.liftParseJSON (`go` c) (`go` listCodec c) value :: JSON.Parser (Map _ _)
      ValueCodec -> pure (value :: JSON.Value)
      EqCodec expected c -> do
        actual <- go value c
        if expected == actual
          then pure actual
          else fail $ unwords ["Expected", show expected, "but got", show actual]
      BimapCodec f _ c -> do
        old <- go value c
        case f old of
          Left err -> fail err
          Right new -> pure new
      EitherCodec u c1 c2 ->
        let leftParser v = Left <$> go v c1
            rightParser v = Right <$> go v c2
         in case u of
              PossiblyJointUnion ->
                case parseEither leftParser value of
                  Right l -> pure l
                  Left err -> prependFailure ("  Previous branch failure: " <> err <> "\n") (rightParser value)
              DisjointUnion ->
                case (parseEither leftParser value, parseEither rightParser value) of
                  (Left _, Right r) -> pure r
                  (Right l, Left _) -> pure l
                  (Right _, Right _) -> fail "Both branches of a disjoint union succeeded."
                  (Left lErr, Left rErr) ->
                    fail $
                      unlines
                        [ "Both branches of a disjoint union failed: ",
                          unwords ["Left:  ", lErr],
                          unwords ["Right: ", rErr]
                        ]
      DiscriminatedUnionCodec propertyName _ m -> do
        discriminatorValue <- (value :: JSON.Object) JSON..: Compat.toKey propertyName
        case HashMap.lookup discriminatorValue m of
          Nothing -> fail $ "Unexpected discriminator value: " <> show discriminatorValue
          Just (_, c) ->
            go value c
      CommentCodec _ c -> go value c
      ReferenceCodec _ c -> go value c
      RequiredKeyCodec k c _ -> do
        valueAtKey <- (value :: JSON.Object) JSON..: Compat.toKey k
        go valueAtKey c JSON.<?> Key (Compat.toKey k)
      OptionalKeyCodec k c _ -> do
        let key = Compat.toKey k
            mValueAtKey = Compat.lookupKey key (value :: JSON.Object)
        forM mValueAtKey $ \valueAtKey -> go (valueAtKey :: JSON.Value) c JSON.<?> Key key
      OptionalKeyWithDefaultCodec k c defaultValue _ -> do
        let key = Compat.toKey k
            mValueAtKey = Compat.lookupKey key (value :: JSON.Object)
        case mValueAtKey of
          Nothing -> pure defaultValue
          Just valueAtKey -> go (valueAtKey :: JSON.Value) c JSON.<?> Key key
      OptionalKeyWithOmittedDefaultCodec k c defaultValue mDoc -> go value $ OptionalKeyWithDefaultCodec k c defaultValue mDoc
      PureCodec a -> pure a
      ApCodec ocf oca -> go (value :: JSON.Object) ocf <*> go (value :: JSON.Object) oca

instance (HasCodec a) => JSON.FromJSON (Autodocodec a) where
  parseJSON = fmap Autodocodec <$> parseJSONViaCodec
