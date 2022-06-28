{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Yaml.Schema where

import Autodocodec
import Autodocodec.Schema
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Yaml as Yaml
import Text.Colour

-- | Render a human-readable schema for a type's 'codec', in colour.
renderColouredSchemaViaCodec :: forall a. HasCodec a => Text
renderColouredSchemaViaCodec = renderColouredSchemaVia (codec @a)

-- | Render a human-readable schema for a given codec, in colour.
renderColouredSchemaVia :: ValueCodec input output -> Text
renderColouredSchemaVia = renderChunksText With24BitColours . schemaChunksVia

-- | Render a human-readable schema for a type's 'codec', without colour.
renderPlainSchemaViaCodec :: forall a. HasCodec a => Text
renderPlainSchemaViaCodec = renderPlainSchemaVia (codec @a)

-- | Render a human-readable schema for a given codec, without colour.
renderPlainSchemaVia :: ValueCodec input output -> Text
renderPlainSchemaVia = renderChunksText WithoutColours . schemaChunksVia

-- | Produce potentially-coloured 'Chunk's for a human-readable schema for a type's 'codec'.
schemaChunksViaCodec :: forall a. HasCodec a => [Chunk]
schemaChunksViaCodec = schemaChunksVia (codec @a)

-- | Produce potentially-coloured 'Chunk's for a human-readable schema for a given codec.
schemaChunksVia :: ValueCodec input output -> [Chunk]
schemaChunksVia = jsonSchemaChunks . jsonSchemaVia

-- | Render a 'JSONSchema' as 'Chunk's
jsonSchemaChunks :: JSONSchema -> [Chunk]
jsonSchemaChunks = concatMap (\l -> l ++ ["\n"]) . go
  where
    indent :: [[Chunk]] -> [[Chunk]]
    indent = map ("  " :)

    addInFrontOfFirstInList :: [Chunk] -> [[Chunk]] -> [[Chunk]]
    addInFrontOfFirstInList cs = \case
      [] -> [cs] -- Shouldn't happen, but fine if it doesn't
      (l : ls) -> (cs ++ l) : indent ls

    jsonValueChunks :: Yaml.Value -> [[Chunk]]
    jsonValueChunks v = map ((: []) . chunk) $ T.lines $ T.strip $ TE.decodeUtf8With TE.lenientDecode (Yaml.encode v)

    docToLines :: Text -> [[Chunk]]
    docToLines doc = map (\line -> [chunk "# ", chunk line]) (T.lines doc)

    choiceChunks :: NonEmpty [[Chunk]] -> [[Chunk]]
    choiceChunks = \case
      chunks :| [] -> addInFrontOfFirstInList ["[ "] chunks ++ [["]"]]
      (chunks :| restChunks) ->
        concat $
          addInFrontOfFirstInList ["[ "] chunks :
          map (addInFrontOfFirstInList [", "]) restChunks ++ [[["]"]]]

    anyOfChunks :: NonEmpty [[Chunk]] -> [[Chunk]]
    anyOfChunks = (["# ", fore green "any of"] :) . choiceChunks

    oneOfChunks :: NonEmpty [[Chunk]] -> [[Chunk]]
    oneOfChunks = (["# ", fore green "one of"] :) . choiceChunks

    orNullChunks :: JSONSchema -> [[Chunk]]
    orNullChunks = (["# ", fore green "or null"] :) . go

    go :: JSONSchema -> [[Chunk]]
    go = \case
      AnySchema -> [[fore yellow "<any>"]]
      NullSchema -> [[fore yellow "null"]]
      BoolSchema -> [[fore yellow "<boolean>"]]
      StringSchema -> [[fore yellow "<string>"]]
      NumberSchema mBounds -> case mBounds of
        Nothing -> [[fore yellow "<number>"]]
        Just NumberBounds {..} ->
          let scientificChunk s = chunk $
                T.pack $ case floatingOrInteger s of
                  Left (_ :: Double) -> show (s :: Scientific)
                  Right i -> show (i :: Integer)
           in [ [ fore yellow "<number>",
                  " # between ",
                  fore green $ scientificChunk numberBoundsLower,
                  " and ",
                  fore green $ scientificChunk numberBoundsUpper
                ]
              ]
      ArraySchema s ->
        let addListMarker = addInFrontOfFirstInList ["- "]
         in addListMarker $ go s
      MapSchema s ->
        addInFrontOfFirstInList [fore white "<key>", ": "] $ [] : go s
      ObjectSchema os -> goObject os
      ValueSchema v -> jsonValueChunks v
      AnyOfSchema ne -> case ne of
        (NullSchema :| [s]) -> orNullChunks s
        (s :| [NullSchema]) -> orNullChunks s
        _ -> anyOfChunks $ NE.map go ne
      OneOfSchema ne -> case ne of
        (NullSchema :| [s]) -> orNullChunks s
        (s :| [NullSchema]) -> orNullChunks s
        _ -> oneOfChunks $ NE.map go ne
      CommentSchema comment s -> docToLines comment ++ go s
      RefSchema name -> [[fore cyan $ chunk $ "ref: " <> name]]
      WithDefSchema defs (RefSchema _) -> concatMap (\(name, s') -> [fore cyan $ chunk $ "def: " <> name] : go s') (M.toList defs)
      WithDefSchema defs s -> concatMap (\(name, s') -> [fore cyan $ chunk $ "def: " <> name] : go s') (M.toList defs) ++ go s

    goObject :: ObjectSchema -> [[Chunk]]
    goObject = \case
      ObjectAnySchema -> [["<object>"]]
      ObjectKeySchema k kr ks mdoc ->
        let requirementComment = \case
              Required -> fore red "required"
              Optional _ -> fore blue "optional"
            mDefaultValue = \case
              Required -> Nothing
              Optional mdv -> mdv
         in let keySchemaChunks = go ks
                defaultValueLine = case mDefaultValue kr of
                  Nothing -> []
                  Just defaultValue ->
                    case jsonValueChunks defaultValue of
                      [c] -> [chunk "# default: " : map (fore magenta) c]
                      cs -> [chunk "# default: "] : map ((chunk "#   " :) . map (fore magenta)) cs
                prefixLines = ["# ", requirementComment kr] : defaultValueLine ++ maybe [] docToLines mdoc
             in addInFrontOfFirstInList [fore white $ chunk k, ": "] (prefixLines ++ keySchemaChunks)
      ObjectAllOfSchema ne -> concatMap goObject $ NE.toList ne
      ObjectAnyOfSchema ne -> anyOfChunks $ NE.map goObject ne
      ObjectOneOfSchema ne -> oneOfChunks $ NE.map goObject ne
