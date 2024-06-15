{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Yaml.Schema
  ( renderColouredSchemaViaCodec,
    renderColouredSchemaVia,
    renderPlainSchemaViaCodec,
    renderPlainSchemaVia,
    schemaChunksViaCodec,
    schemaChunksVia,
    jsonSchemaChunks,
    jsonSchemaChunkLines,
  )
where

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
import Data.Word
import Data.Yaml as Yaml
import Text.Colour

-- | Render a human-readable schema for a type's 'codec', in colour.
renderColouredSchemaViaCodec :: forall a. (HasCodec a) => Text
renderColouredSchemaViaCodec = renderColouredSchemaVia (codec @a)

-- | Render a human-readable schema for a given codec, in colour.
renderColouredSchemaVia :: ValueCodec input output -> Text
renderColouredSchemaVia = renderChunksText With24BitColours . schemaChunksVia

-- | Render a human-readable schema for a type's 'codec', without colour.
renderPlainSchemaViaCodec :: forall a. (HasCodec a) => Text
renderPlainSchemaViaCodec = renderPlainSchemaVia (codec @a)

-- | Render a human-readable schema for a given codec, without colour.
renderPlainSchemaVia :: ValueCodec input output -> Text
renderPlainSchemaVia = renderChunksText WithoutColours . schemaChunksVia

-- | Produce potentially-coloured 'Chunk's for a human-readable schema for a type's 'codec'.
schemaChunksViaCodec :: forall a. (HasCodec a) => [Chunk]
schemaChunksViaCodec = schemaChunksVia (codec @a)

-- | Produce potentially-coloured 'Chunk's for a human-readable schema for a given codec.
schemaChunksVia :: ValueCodec input output -> [Chunk]
schemaChunksVia = jsonSchemaChunks . jsonSchemaVia

-- | Render a 'JSONSchema' as 'Chunk's
jsonSchemaChunks :: JSONSchema -> [Chunk]
jsonSchemaChunks = unlinesChunks . jsonSchemaChunkLines

-- | Render a 'JSONSchema' as lines of 'Chunk's
jsonSchemaChunkLines :: JSONSchema -> [[Chunk]]
jsonSchemaChunkLines = go
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
          addInFrontOfFirstInList ["[ "] chunks
            : map (addInFrontOfFirstInList [", "]) restChunks
            ++ [[["]"]]]

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
        Just nb -> numberBoundsChunks nb
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

    numberBoundsChunks :: NumberBounds -> [[Chunk]]
    numberBoundsChunks nb =
      [ [fore yellow "<number>", " # "] ++ case guessNumberBoundsSymbolic nb of
          BitUInt w ->
            [ fore green $ chunk $ T.pack $ show w <> " bit unsigned integer"
            ]
          BitSInt w ->
            [ fore green $ chunk $ T.pack $ show w <> " bit signed integer"
            ]
          OtherNumberBounds l u ->
            [ "between ",
              fore green $ scientificChunk l,
              " and ",
              fore green $ scientificChunk u
            ]
      ]
      where
        scientificChunk = \case
          Zero -> "0"
          PowerOf2 w -> chunk $ T.pack $ "2^" <> show w
          PowerOf2MinusOne w -> chunk $ T.pack $ "2^" <> show w <> "-1"
          MinusPowerOf2 w -> chunk $ T.pack $ "-2^" <> show w
          MinusPowerOf2MinusOne w -> chunk $ T.pack $ "- (2^" <> show w <> "-1)"
          OtherInteger i -> chunk $ T.pack $ show i
          OtherDouble d -> chunk $ T.pack $ show d

data NumberBoundsSymbolic
  = BitUInt !Word8 -- w bit unsigned int
  | BitSInt !Word8 -- w bit signed int
  | OtherNumberBounds !ScientificSymbolic !ScientificSymbolic

guessNumberBoundsSymbolic :: NumberBounds -> NumberBoundsSymbolic
guessNumberBoundsSymbolic NumberBounds {..} =
  case (guessScientificSymbolic numberBoundsLower, guessScientificSymbolic numberBoundsUpper) of
    (Zero, PowerOf2MinusOne w) -> BitUInt w
    (MinusPowerOf2 w1, PowerOf2MinusOne w2) | w1 == w2 -> BitSInt (succ w1)
    (l, u) -> OtherNumberBounds l u

data ScientificSymbolic
  = Zero
  | PowerOf2 !Word8 -- 2^w
  | PowerOf2MinusOne !Word8 -- 2^w -1
  | MinusPowerOf2 !Word8 -- - 2^w
  | MinusPowerOf2MinusOne !Word8 -- - (2^w -1)
  | OtherInteger !Integer
  | OtherDouble !Double

guessScientificSymbolic :: Scientific -> ScientificSymbolic
guessScientificSymbolic s = case floatingOrInteger s of
  Left d -> OtherDouble d
  Right i ->
    let log2Rounded :: Word8
        log2Rounded = round (logBase 2 (fromInteger (abs i)) :: Double)
        guess :: Integer
        guess = 2 ^ log2Rounded
     in if
          | i == 0 -> Zero
          | guess == i -> PowerOf2 log2Rounded
          | (guess - 1) == i -> PowerOf2MinusOne log2Rounded
          | -guess == i -> MinusPowerOf2 log2Rounded
          | -(guess - 1) == i -> MinusPowerOf2MinusOne log2Rounded
          | otherwise -> OtherInteger i
