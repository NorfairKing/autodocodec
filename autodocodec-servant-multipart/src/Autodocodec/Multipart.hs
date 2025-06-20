{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Autodocodec.Multipart where

import Autodocodec
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Coerce (coerce)
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Servant.Multipart as Servant
import Servant.Multipart.API as Servant

toMultipartViaCodec :: forall a tag. (HasObjectCodec a) => a -> MultipartData tag
toMultipartViaCodec = toMultipartVia (objectCodec @a)

toMultipartVia :: ObjectCodec a void -> a -> MultipartData tag
toMultipartVia = flip go
  where
    go :: a -> ObjectCodec a void -> MultipartData tag
    go a = \case
      BimapCodec _ to c -> go (to a) c
      EitherCodec _ c1 c2 -> case coerce a of
        Left a1 -> go a1 c1
        Right a2 -> go a2 c2
      DiscriminatedUnionCodec discriminator encoding _ ->
        let (discriminatorValue, c) = encoding a
         in mappendMultipartData
              ( MultipartData
                  { inputs = [Input discriminator discriminatorValue],
                    files = []
                  }
              )
              (go a c)
      RequiredKeyCodec key vc _ ->
        MultipartData
          { inputs = map (Input key) (goValue (coerce a) vc),
            files = []
          }
      OptionalKeyCodec key vc _ ->
        MultipartData
          { inputs = do
              a' <- maybeToList $ coerce a
              v <- goValue a' vc
              pure $ Input key v,
            files = []
          }
      OptionalKeyWithDefaultCodec key vc _ _ ->
        MultipartData
          { inputs = map (Input key) (goValue a vc),
            files = []
          }
      OptionalKeyWithOmittedDefaultCodec key vc defaultValue _ ->
        MultipartData
          { inputs =
              if coerce a == defaultValue
                then []
                else map (Input key) (goValue (coerce a) vc),
            files = []
          }
      PureCodec _ -> memptyMultipartData
      ApCodec oc1 oc2 -> mappendMultipartData (go a oc1) (go a oc2)

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
      BoolCodec _ ->
        case coerce a of
          True -> "True"
          False -> "False"
      StringCodec _ -> coerce a
      vc ->
        let value = toJSONVia vc a
         in case value of
              JSON.String t -> t
              _ -> TE.decodeUtf8 (LB.toStrict (JSON.encode value))

memptyMultipartData :: MultipartData tag
memptyMultipartData =
  MultipartData
    { inputs = [],
      files = []
    }

mappendMultipartData :: MultipartData tag -> MultipartData tag -> MultipartData tag
mappendMultipartData mpd1 mpd2 =
  MultipartData
    { inputs = inputs mpd1 ++ inputs mpd2,
      files = files mpd1 ++ files mpd2
    }

instance (HasObjectCodec a) => Servant.ToMultipart tag (Autodocodec a) where
  toMultipart = toMultipartViaCodec . unAutodocodec

fromMultipartViaCodec :: forall a tag. (HasObjectCodec a) => MultipartData tag -> Either String a
fromMultipartViaCodec = fromMultipartVia (objectCodec @a)

fromMultipartVia :: ObjectCodec void a -> MultipartData tag -> Either String a
fromMultipartVia = flip go
  where
    go :: MultipartData tag -> ObjectCodec void a -> Either String a
    go mpd = \case
      BimapCodec from _ c -> go mpd c >>= from
      EitherCodec u c1 c2 -> coerce $ case u of
        PossiblyJointUnion ->
          case go mpd c1 of
            Right l -> pure (Left l)
            Left err1 -> case go mpd c2 of
              Left err2 -> Left $ "  Previous branch failure: " <> err1 <> "\n" <> err2
              Right r -> pure (Right r)
        DisjointUnion ->
          case (go mpd c1, go mpd c2) of
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
        discriminatorValue <- lookupInput discriminator mpd
        case HashMap.lookup discriminatorValue m of
          Nothing -> Left $ "Unexpected discriminator value: " <> show discriminatorValue
          Just (_, c) -> go mpd c
      RequiredKeyCodec key vc _ -> do
        values <- lookupLInput key mpd
        coerce $ goValue values vc
      OptionalKeyCodec key vc _ -> do
        values <- lookupLInput key mpd
        coerce $ case values of
          [] -> pure Nothing
          _ -> Just <$> goValue values vc
      OptionalKeyWithDefaultCodec key vc defaultValue _ -> do
        values <- lookupLInput key mpd
        coerce $ case values of
          [] -> pure defaultValue
          _ -> goValue values vc
      OptionalKeyWithOmittedDefaultCodec key vc defaultValue _ -> do
        values <- lookupLInput key mpd
        coerce $ case values of
          [] -> pure defaultValue
          _ -> goValue values vc
      PureCodec v -> pure v
      ApCodec ocf oca -> go mpd ocf <*> go mpd oca

    goValue :: [Text] -> ValueCodec void a -> Either String a
    goValue ts = \case
      BimapCodec from _ c -> goValue ts c >>= from
      EitherCodec u c1 c2 -> coerce $ case u of
        PossiblyJointUnion ->
          case goValue ts c1 of
            Right l -> pure (Left l)
            Left err1 -> case goValue ts c2 of
              Left err2 -> Left $ "  Previous branch failure: " <> err1 <> "\n" <> err2
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
        _ -> Left "Expected exactly one value."

    goSingleValue :: Text -> ValueCodec void a -> Either String a
    goSingleValue t = \case
      BimapCodec from _ c -> goSingleValue t c >>= from
      EitherCodec u c1 c2 -> coerce $ case u of
        PossiblyJointUnion ->
          case goSingleValue t c1 of
            Right l -> pure (Left l)
            Left err1 -> case goSingleValue t c2 of
              Left err2 -> Left $ "  Previous branch failure: " <> err1 <> "\n" <> err2
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
        _ -> Left $ "not 'null': " <> show t
      BoolCodec _ -> coerce $ case t of
        "false" -> Right False
        "False" -> Right False
        "true" -> Right True
        "True" -> Right True
        _ -> Left $ "Unknown bool: " <> show t
      StringCodec _ -> Right (coerce t)
      vc -> case JSON.parseEither (parseJSONVia vc) (JSON.String t) of
        Right a -> Right a
        Left _ -> do
          value <- JSON.eitherDecode (LB.fromStrict (TE.encodeUtf8 t))
          JSON.parseEither (parseJSONVia vc) value

lookupMInput :: Text -> MultipartData tag -> Either String (Maybe Text)
lookupMInput iname = Right . fmap iValue . find ((== iname) . iName) . inputs

lookupLInput :: Text -> MultipartData tag -> Either String [Text]
lookupLInput iname = Right . map iValue . filter ((== iname) . iName) . inputs

instance (HasObjectCodec a) => Servant.FromMultipart tag (Autodocodec a) where
  fromMultipart = fmap Autodocodec . fromMultipartViaCodec
