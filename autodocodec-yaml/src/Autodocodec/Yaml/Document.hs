{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Yaml.Document where

import Autodocodec
import Autodocodec.Aeson
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Yaml as Yaml
import Text.Colour

schemaChunksViaCodec :: forall a. HasCodec a => [Chunk]
schemaChunksViaCodec = schemaChunksVia (codec @a)

schemaChunksVia :: Codec input output -> [Chunk]
schemaChunksVia = jsonSchemaChunks . jsonSchemaVia

jsonSchemaChunks :: JSONSchema -> [Chunk]
jsonSchemaChunks = concatMap (\l -> l ++ ["\n"]) . go
  where
    indent :: [[Chunk]] -> [[Chunk]]
    indent = map ("  " :)

    addInFrontOfFirstInList :: [Chunk] -> [[Chunk]] -> [[Chunk]]
    addInFrontOfFirstInList cs = \case
      [] -> [cs] -- Shouldn't happen, but fine if it doesn't
      (l : ls) -> (cs ++ l) : indent ls

    go :: JSONSchema -> [[Chunk]]
    go = \case
      AnySchema -> [[fore yellow "<any>"]]
      NullSchema -> [[fore yellow "null"]]
      BoolSchema -> [[fore yellow "<boolean>"]]
      StringSchema -> [[fore yellow "<string>"]]
      NumberSchema -> [[fore yellow "<number>"]]
      ArraySchema s ->
        let addListMarker = addInFrontOfFirstInList ["- "]
         in addListMarker $ go s
      ObjectSchema s ->
        let requirementComment = \case
              Required -> fore red "required"
              Optional -> fore blue "optional"
            keySchemaFor k (kr, ks) = addInFrontOfFirstInList [fore white $ chunk k, ":", " "] (["# ", requirementComment kr] : go ks)
         in if M.null s
              then [["<object>"]]
              else concatMap (uncurry keySchemaFor) (M.toList s)
      ValueSchema v -> [[chunk $ TE.decodeUtf8With TE.lenientDecode (Yaml.encode v)]]
      ChoiceSchema s ->
        let addListAround = \case
              s_ :| [] -> addInFrontOfFirstInList ["[ "] (go s_) ++ [["]"]]
              (s_ :| rest) ->
                concat $
                  addInFrontOfFirstInList ["[ "] (go s_) :
                  map (addInFrontOfFirstInList [", "] . go) rest
                    ++ [[["]"]]]
         in addListAround s
      CommentSchema c s -> [chunk $ "# " <> c] : go s
