{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Yaml.Document where

import Autodocodec
import Autodocodec.Aeson
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Yaml as Yaml
import Text.Colour

schemaChunksViaCodec :: forall a. HasCodec a => [Chunk]
schemaChunksViaCodec = schemaChunksVia (codec @a)

schemaChunksVia :: ValueCodec input output -> [Chunk]
schemaChunksVia = jsonSchemaChunks . jsonSchemaVia

jsonSchemaChunks :: JSONSchema -> [Chunk]
jsonSchemaChunks = concatMap (\l -> l ++ ["\n"]) . (`evalState` S.empty) . go
  where
    indent :: [[Chunk]] -> [[Chunk]]
    indent = map ("  " :)

    addInFrontOfFirstInList :: [Chunk] -> [[Chunk]] -> [[Chunk]]
    addInFrontOfFirstInList cs = \case
      [] -> [cs] -- Shouldn't happen, but fine if it doesn't
      (l : ls) -> (cs ++ l) : indent ls

    go :: JSONSchema -> State (Set Text) [[Chunk]]
    go = \case
      AnySchema -> pure [[fore yellow "<any>"]]
      NullSchema -> pure [[fore yellow "null"]]
      BoolSchema -> pure [[fore yellow "<boolean>"]]
      StringSchema -> pure [[fore yellow "<string>"]]
      NumberSchema -> pure [[fore yellow "<number>"]]
      ArraySchema s ->
        let addListMarker = addInFrontOfFirstInList ["- "]
         in addListMarker <$> go s
      ObjectSchema s ->
        let requirementComment = \case
              Required -> fore red "required"
              Optional _ -> fore blue "optional"
            mDefaultValue = \case
              Required -> Nothing
              Optional mdv -> fst <$> mdv
            keySchemaFor k (kr, ks, mdoc) = do
              keySchemaChunks <- go ks
              let docToLines :: Text -> [[Chunk]]
                  docToLines doc = map (\line -> [chunk "# ", chunk line]) (T.lines doc)
                  defaultValueLine = case mDefaultValue kr of
                    Nothing -> []
                    Just defaultValue -> [[chunk "# default: ", fore magenta $ chunk defaultValue]]
              let prefixLines = ["# ", requirementComment kr] : defaultValueLine ++ maybe [] docToLines mdoc
              pure $ addInFrontOfFirstInList [fore white $ chunk k, ":", " "] (prefixLines ++ keySchemaChunks)
         in if null s
              then pure [["<object>"]]
              else concat <$> mapM (uncurry keySchemaFor) s
      ValueSchema v -> pure [[chunk $ T.strip $ TE.decodeUtf8With TE.lenientDecode (Yaml.encode v)]]
      ChoiceSchema s ->
        let addListAround = \case
              s_ :| [] -> do
                chunks <- go s_
                pure $ addInFrontOfFirstInList ["[ "] chunks ++ [["]"]]
              (s_ :| rest) -> do
                chunks <- go s_
                restChunks <- mapM go rest
                pure $
                  concat $
                    addInFrontOfFirstInList ["[ "] chunks :
                    map (addInFrontOfFirstInList [", "]) restChunks
                      ++ [[["]"]]]
         in addListAround s
      DefaultSchema shownValue _ s -> ([chunk "# default: ", fore magenta $ chunk shownValue] :) <$> go s
      CommentSchema comment s -> ([chunk $ "# " <> comment] :) <$> go s
      ReferenceSchema name s -> do
        alreadySeen <- gets (S.member name)
        if alreadySeen
          then pure [[fore cyan $ chunk $ "ref: " <> name]]
          else do
            modify (S.insert name)
            ([fore cyan $ chunk $ "def: " <> name] :) <$> go s
