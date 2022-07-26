{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Multipart where

import Autodocodec
import qualified Data.HashMap.Strict as HashMap
import Servant.Multipart

toMultipartViaCodec :: forall a. HasObjectCodec a => a -> MultipartData Tmp
toMultipartViaCodec = toMultipartVia (objectCodec @a)

toMultipartVia :: ObjectCodec a void -> a -> MultipartData Tmp
toMultipartVia = flip go
  where
    go :: a -> ObjectCodec a void -> MultipartData Tmp
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
      _ -> undefined

mappendMultipartData :: MultipartData tag -> MultipartData tag -> MultipartData tag
mappendMultipartData mpd1 mpd2 =
  MultipartData
    { inputs = inputs mpd1 ++ inputs mpd2,
      files = files mpd1 ++ files mpd2
    }

fromMultipartViaCodec :: forall a. HasObjectCodec a => MultipartData Tmp -> Either String a
fromMultipartViaCodec = fromMultipartVia (objectCodec @a)

fromMultipartVia :: ObjectCodec void a -> MultipartData Tmp -> Either String a
fromMultipartVia = flip go
  where
    go :: MultipartData Tmp -> ObjectCodec void a -> Either String a
    go mpd@MultipartData {..} = \case
      BimapCodec from _ c -> go mpd c >>= from
      EitherCodec u c1 c2 -> case u of
        PossiblyJointUnion ->
          case go mpd c1 of
            Right l -> pure (Left l)
            Left err -> Right <$> go mpd c2
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
      _ -> undefined
