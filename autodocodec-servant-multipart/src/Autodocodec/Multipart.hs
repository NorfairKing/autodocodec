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
import Control.Monad
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HashMap
import Data.List (find)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Servant.Multipart as Servant
import Servant.Multipart.API as Servant

toMultipartViaCodec :: forall a tag. HasObjectCodec a => a -> MultipartData tag
toMultipartViaCodec = toMultipartVia (objectCodec @a)

toMultipartVia :: ObjectCodec a void -> a -> MultipartData tag
toMultipartVia = flip go
  where
    go :: a -> ObjectCodec a void -> MultipartData tag
    go a = \case
      BimapCodec _ to c -> go (to a) c
      EitherCodec _ c1 c2 -> case a of
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
          { inputs = [Input key (goValue a vc)],
            files = []
          }
      OptionalKeyCodec key vc _ ->
        MultipartData
          { inputs = [Input key (goValue v vc) | v <- maybeToList a],
            files = []
          }
      OptionalKeyWithDefaultCodec key vc _ _ ->
        MultipartData
          { inputs = [Input key (goValue a vc)],
            files = []
          }
      OptionalKeyWithOmittedDefaultCodec key vc defaultValue _ ->
        MultipartData
          { inputs = [Input key (goValue a vc) | a /= defaultValue],
            files = []
          }
      PureCodec _ -> memptyMultipartData
      ApCodec oc1 oc2 -> mappendMultipartData (go a oc1) (go a oc2)

    goValue :: a -> ValueCodec a void -> Text
    goValue a = \case
      vc -> TE.decodeUtf8 (LB.toStrict (JSON.encode (toJSONVia vc a)))

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

instance HasObjectCodec a => Servant.ToMultipart tag (Autodocodec a) where
  toMultipart = toMultipartViaCodec . unAutodocodec

fromMultipartViaCodec :: forall a tag. HasObjectCodec a => MultipartData tag -> Either String a
fromMultipartViaCodec = fromMultipartVia (objectCodec @a)

fromMultipartVia :: ObjectCodec void a -> MultipartData tag -> Either String a
fromMultipartVia = flip go
  where
    go :: MultipartData tag -> ObjectCodec void a -> Either String a
    go mpd = \case
      BimapCodec from _ c -> go mpd c >>= from
      EitherCodec u c1 c2 -> case u of
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
        value <- lookupInput key mpd
        goValue value vc
      OptionalKeyCodec key vc _ -> do
        mValue <- lookupMInput key mpd
        forM mValue $ \value ->
          goValue value vc
      OptionalKeyWithDefaultCodec key vc defaultValue _ -> do
        mValue <- lookupMInput key mpd
        case mValue of
          Nothing -> pure defaultValue
          Just value -> goValue value vc
      OptionalKeyWithOmittedDefaultCodec key vc defaultValue _ -> do
        mValue <- lookupMInput key mpd
        case mValue of
          Nothing -> pure defaultValue
          Just value -> goValue value vc
      PureCodec v -> pure v
      ApCodec ocf oca -> go mpd ocf <*> go mpd oca

    goValue :: Text -> ValueCodec void a -> Either String a
    goValue t = \case
      vc ->
        JSON.eitherDecode (LB.fromStrict (TE.encodeUtf8 t))
          >>= JSON.parseEither (parseJSONVia vc)

lookupMInput :: Text -> MultipartData tag -> Either String (Maybe Text)
lookupMInput iname = Right . fmap iValue . find ((== iname) . iName) . inputs

lookupLInput :: Text -> MultipartData tag -> Either String [Text]
lookupLInput iname = Right . map iValue . filter ((== iname) . iName) . inputs

instance HasObjectCodec a => Servant.FromMultipart tag (Autodocodec a) where
  fromMultipart = fmap Autodocodec . fromMultipartViaCodec
