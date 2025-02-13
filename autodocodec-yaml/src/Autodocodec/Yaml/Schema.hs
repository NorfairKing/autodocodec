{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Yaml.Schema
  ( renderColouredSchemaViaCodec,
    renderColouredSchemaVia,
    renderColouredObjectSchemaViaCodec,
    renderColouredObjectSchemaVia,
    renderPlainSchemaViaCodec,
    renderPlainSchemaVia,
    renderPlainObjectSchemaViaCodec,
    renderPlainObjectSchemaVia,
    schemaChunksViaCodec,
    objectSchemaChunksViaCodec,
    schemaChunksVia,
    objectSchemaChunksVia,
    jsonSchemaChunks,
    jsonObjectSchemaChunks,
    jsonSchemaChunkLines,
    jsonObjectSchemaChunkLines,
  )
where

import Autodocodec
import Autodocodec.Schema
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Yaml as Yaml
import Text.Colour

-- | Render a human-readable schema for a type's 'codec', in colour.
renderColouredSchemaViaCodec :: forall a. (HasCodec a) => Text
renderColouredSchemaViaCodec = renderColouredSchemaVia (codec @a)

-- | Render a human-readable schema for a type's 'objectCodec', in colour.
renderColouredObjectSchemaViaCodec :: forall a. (HasObjectCodec a) => Text
renderColouredObjectSchemaViaCodec = renderColouredObjectSchemaVia (objectCodec @a)

-- | Render a human-readable schema for a given codec, in colour.
renderColouredSchemaVia :: ValueCodec input output -> Text
renderColouredSchemaVia = renderChunksText With24BitColours . schemaChunksVia

-- | Render a human-readable schema for a given object codec, in colour.
renderColouredObjectSchemaVia :: ObjectCodec input output -> Text
renderColouredObjectSchemaVia = renderChunksText With24BitColours . objectSchemaChunksVia

-- | Render a human-readable schema for a type's 'codec', without colour.
renderPlainSchemaViaCodec :: forall a. (HasCodec a) => Text
renderPlainSchemaViaCodec = renderPlainSchemaVia (codec @a)

-- | Render a human-readable schema for a type's 'objectCodec', without colour.
renderPlainObjectSchemaViaCodec :: forall a. (HasObjectCodec a) => Text
renderPlainObjectSchemaViaCodec = renderPlainObjectSchemaVia (objectCodec @a)

-- | Render a human-readable schema for a given codec, without colour.
renderPlainSchemaVia :: ValueCodec input output -> Text
renderPlainSchemaVia = renderChunksText WithoutColours . schemaChunksVia

-- | Render a human-readable schema for a given object codec, without colour.
renderPlainObjectSchemaVia :: ObjectCodec input output -> Text
renderPlainObjectSchemaVia = renderChunksText WithoutColours . objectSchemaChunksVia

-- | Produce potentially-coloured 'Chunk's for a human-readable schema for a type's 'codec'.
schemaChunksViaCodec :: forall a. (HasCodec a) => [Chunk]
schemaChunksViaCodec = schemaChunksVia (codec @a)

-- | Produce potentially-coloured 'Chunk's for a human-readable schema for a type's 'objectCodec'.
objectSchemaChunksViaCodec :: forall a. (HasObjectCodec a) => [Chunk]
objectSchemaChunksViaCodec = objectSchemaChunksVia (objectCodec @a)

-- | Produce potentially-coloured 'Chunk's for a human-readable schema for a given codec.
schemaChunksVia :: ValueCodec input output -> [Chunk]
schemaChunksVia = jsonSchemaChunks . jsonSchemaVia

-- | Produce potentially-coloured 'Chunk's for a human-readable schema for a given object codec.
objectSchemaChunksVia :: ObjectCodec input output -> [Chunk]
objectSchemaChunksVia = jsonObjectSchemaChunks . jsonObjectSchemaVia

-- | Render a 'JSONSchema' as 'Chunk's
jsonSchemaChunks :: JSONSchema -> [Chunk]
jsonSchemaChunks = unlinesChunks . jsonSchemaChunkLines

-- | Render an 'ObjectSchema' as 'Chunk's
jsonObjectSchemaChunks :: ObjectSchema -> [Chunk]
jsonObjectSchemaChunks = unlinesChunks . jsonObjectSchemaChunkLines

-- | Render a 'JSONSchema' as lines of 'Chunk's
jsonSchemaChunkLines :: JSONSchema -> [[Chunk]]
jsonSchemaChunkLines = goValue

-- | Render an 'ObjectSchema' as lines of 'Chunk's
jsonObjectSchemaChunkLines :: ObjectSchema -> [[Chunk]]
jsonObjectSchemaChunkLines = goObject

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
      addInFrontOfFirstInList ["[ "] chunks
        : map (addInFrontOfFirstInList [", "]) restChunks
        ++ [[["]"]]]

anyOfChunks :: NonEmpty [[Chunk]] -> [[Chunk]]
anyOfChunks = (["# ", fore green "any of"] :) . choiceChunks

oneOfChunks :: NonEmpty [[Chunk]] -> [[Chunk]]
oneOfChunks = (["# ", fore green "one of"] :) . choiceChunks

orNullChunks :: JSONSchema -> [[Chunk]]
orNullChunks = (["# ", fore green "or null"] :) . goValue

goValue :: JSONSchema -> [[Chunk]]
goValue = \case
  AnySchema -> [[fore yellow "<any>"]]
  NullSchema -> [[fore yellow "null"]]
  BoolSchema -> [[fore yellow "<boolean>"]]
  StringSchema -> [[fore yellow "<string>"]]
  IntegerSchema bounds -> integerBoundsChunks bounds
  NumberSchema _ -> [[fore yellow "<number>"]] -- TODO bounds?
  ArraySchema s ->
    let addListMarker = addInFrontOfFirstInList ["- "]
     in addListMarker $ goValue s
  MapSchema s ->
    addInFrontOfFirstInList [fore white "<key>", ": "] $ [] : goValue s
  ObjectSchema os -> goObject os
  ValueSchema v -> jsonValueChunks v
  AnyOfSchema ne -> case ne of
    (NullSchema :| [s]) -> orNullChunks s
    (s :| [NullSchema]) -> orNullChunks s
    _ -> anyOfChunks $ NE.map goValue ne
  OneOfSchema ne -> case ne of
    (NullSchema :| [s]) -> orNullChunks s
    (s :| [NullSchema]) -> orNullChunks s
    _ -> oneOfChunks $ NE.map goValue ne
  CommentSchema comment s -> docToLines comment ++ goValue s
  RefSchema name -> [[fore cyan $ chunk $ "ref: " <> name]]
  WithDefSchema defs (RefSchema _) -> concatMap (\(name, s') -> [fore cyan $ chunk $ "def: " <> name] : goValue s') (M.toList defs)
  WithDefSchema defs s -> concatMap (\(name, s') -> [fore cyan $ chunk $ "def: " <> name] : goValue s') (M.toList defs) ++ goValue s
  where
    integerBoundsChunks :: Bounds Integer -> [[Chunk]]
    integerBoundsChunks nb =
      [ fore yellow "<integer>" : case guessIntegerBoundsSymbolic nb of
          BitUInt w ->
            [ " # ",
              fore green $ chunk $ T.pack $ show w <> " bit unsigned integer"
            ]
          BitSInt w ->
            [ " # ",
              fore green $ chunk $ T.pack $ show w <> " bit signed integer"
            ]
          OtherIntegerBounds ml mu -> case (ml, mu) of
            (Nothing, Nothing) -> []
            (Just l, Nothing) -> [fore green $ integerChunk l, " or more"]
            (Nothing, Just l) -> [fore green $ integerChunk l, " or less"]
            (Just l, Just u) ->
              [ "between ",
                fore green $ integerChunk l,
                " and ",
                fore green $ integerChunk u
              ]
      ]
      where
        integerChunk = \case
          Zero -> "0"
          PowerOf2 w -> chunk $ T.pack $ "2^" <> show w
          PowerOf2MinusOne w -> chunk $ T.pack $ "2^" <> show w <> "-1"
          MinusPowerOf2 w -> chunk $ T.pack $ "-2^" <> show w
          MinusPowerOf2MinusOne w -> chunk $ T.pack $ "- (2^" <> show w <> "-1)"
          OtherInteger i -> chunk $ T.pack $ show i

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
     in let keySchemaChunks = goValue ks
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
