{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-duplicate-exports #-}

module Autodocodec.Nix
  ( -- * Producing a Nixos module type
    renderNixOptionTypeViaCodec,
    renderNixOptionsViaCodec,
    renderNixOptionTypeVia,
    renderNixOptionsVia,
    valueCodecNixOptionType,
    objectCodecNixOption,
    Option (..),
    OptionType (..),
    renderOption,
    renderOptionType,

    -- * To makes sure we definitely export everything.
    module Autodocodec.Nix,
  )
where

import Autodocodec
import Data.Containers.ListUtils
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

renderNixOptionTypeViaCodec :: forall a. (HasCodec a) => Text
renderNixOptionTypeViaCodec = renderNixOptionTypeVia (codec @a)

renderNixOptionsViaCodec :: forall a. (HasObjectCodec a) => Text
renderNixOptionsViaCodec = renderNixOptionsVia (objectCodec @a)

renderNixOptionTypeVia :: ValueCodec input output -> Text
renderNixOptionTypeVia =
  T.unlines
    . renderOptionType
    . simplifyOptionType
    . fromMaybe (OptionTypeSimple "types.anything")
    . valueCodecNixOptionType

renderNixOptionsVia :: ObjectCodec input output -> Text
renderNixOptionsVia =
  T.unlines
    . renderOptions
    . simplifyOptions
    . objectCodecNixOption

valueCodecNixOptionType :: ValueCodec input output -> Maybe OptionType
valueCodecNixOptionType = go
  where
    mTyp = fromMaybe $ OptionTypeSimple "types.anything"
    go :: ValueCodec input output -> Maybe OptionType
    go = \case
      NullCodec -> Nothing
      BoolCodec _ -> Just $ OptionTypeSimple "types.bool"
      StringCodec _ -> Just $ OptionTypeSimple "types.str"
      NumberCodec _ mBounds -> Just $ OptionTypeSimple $ case mBounds of
        Nothing -> "types.number"
        Just bounds -> case guessNumberBoundsSymbolic bounds of
          BitUInt w -> T.pack $ "types.u" <> show w -- TODO this will not exist for u7
          BitSInt w -> T.pack $ "types.s" <> show w -- TODO this will not exist for s7
          OtherNumberBounds _ _ -> "types.number" -- TODO
      HashMapCodec c -> Just $ OptionTypeAttrsOf $ mTyp $ go c
      MapCodec c -> Just $ OptionTypeAttrsOf $ mTyp $ go c
      ValueCodec -> Just (OptionTypeSimple "types.unspecified")
      ArrayOfCodec _ c -> Just $ OptionTypeListOf $ mTyp $ go c
      ObjectOfCodec _ oc -> Just (OptionTypeSubmodule (objectCodecNixOption oc))
      EqCodec _ _ -> Nothing -- TODO
      BimapCodec _ _ c -> go c
      EitherCodec _ c1 c2 -> Just $ OptionTypeOneOf (map mTyp [go c1, go c2])
      CommentCodec _ c -> go c
      ReferenceCodec {} -> Nothing -- TODO: let-binding?

objectCodecNixOption :: ObjectCodec input output -> Map Text Option
objectCodecNixOption = go
  where
    go :: ObjectCodec input output -> Map Text Option
    go = \case
      DiscriminatedUnionCodec k _ m ->
        M.insert
          k
          ( Option
              { optionType = Just $ OptionTypeOneOf $ map (OptionTypeSimple . T.pack . show) $ HM.keys m,
                optionDescription = Nothing
              }
          )
          $ M.unionsWith
            ( \t1 t2 ->
                Option
                  { optionType = Just $ OptionTypeOneOf $ map (fromMaybe (OptionTypeSimple "types.anything") . optionType) [t1, t2],
                    optionDescription = Nothing -- TODO
                  }
            )
          $ map (go . snd)
          $ HM.elems m
      RequiredKeyCodec key o mDesc ->
        M.singleton key $
          Option
            { optionType = valueCodecNixOptionType o,
              optionDescription = mDesc
            } -- TODO use the docs
      OptionalKeyCodec key o mDesc ->
        M.singleton key $
          Option
            { optionType = valueCodecNixOptionType o,
              optionDescription = mDesc
            } -- TODO mark as optional
      OptionalKeyWithDefaultCodec key o _ mDesc ->
        -- TODO set the default
        M.singleton
          key
          Option
            { optionType = valueCodecNixOptionType o,
              optionDescription = mDesc
            }
      OptionalKeyWithOmittedDefaultCodec key o _ mDesc ->
        -- TODO set the default
        M.singleton
          key
          Option
            { optionType = valueCodecNixOptionType o,
              optionDescription = mDesc
            }
      PureCodec _ -> M.empty
      ApCodec c1 c2 -> M.union (go c1) (go c2)
      BimapCodec _ _ c -> go c
      EitherCodec _ c1 c2 -> M.union (go c1) (go c2) -- TODO use an or?

data Option = Option
  { optionType :: !(Maybe OptionType),
    optionDescription :: !(Maybe Text)
  }
  deriving (Eq, Ord)

emptyOption :: Option
emptyOption =
  Option
    { optionType = Nothing,
      optionDescription = Nothing
    }

simplifyOption :: Option -> Option
simplifyOption o = o {optionType = simplifyOptionType <$> optionType o}

renderOption :: Option -> [Text]
renderOption Option {..} =
  concat
    [ ["mkOption {"],
      indent $
        concat
          [ concat
              [ prepend "type = " (renderOptionType typ `append` ";") | typ <- maybeToList optionType
              ],
            concat
              [ prepend "description = " ([T.pack (show d)] `append` ";") | d <- maybeToList optionDescription
              ]
          ],
      ["}"]
    ]

data OptionType
  = OptionTypeSimple !Text
  | OptionTypeListOf !OptionType
  | OptionTypeAttrsOf !OptionType
  | OptionTypeOneOf ![OptionType]
  | OptionTypeSubmodule !(Map Text Option)
  deriving (Eq, Ord)

simplifyOptionType :: OptionType -> OptionType
simplifyOptionType = go
  where
    go = \case
      OptionTypeSimple t -> OptionTypeSimple t
      OptionTypeListOf o -> OptionTypeListOf $ go o
      OptionTypeAttrsOf o -> OptionTypeAttrsOf $ go o
      OptionTypeOneOf os -> case nubOrd $ concatMap goOr os of
        [ot] -> ot
        os' -> OptionTypeOneOf os'
      OptionTypeSubmodule m -> OptionTypeSubmodule $ M.map goOpt m

    goOpt o = o {optionType = go <$> optionType o}

    goOr = \case
      OptionTypeOneOf os -> concatMap goOr os
      o -> [o]

renderOptionType :: OptionType -> [Text]
renderOptionType = \case
  OptionTypeSimple t -> [t]
  OptionTypeListOf o -> prepend "types.listOf (" (renderOptionType o) `append` ")"
  OptionTypeAttrsOf o -> prepend "types.attrsOf (" (renderOptionType o) `append` ")"
  OptionTypeOneOf os -> prepend "types.oneOf [" (concatMap (parens . renderOptionType) os) `append` "]"
  OptionTypeSubmodule obj ->
    prepend
      "types.submodule { options = "
      (renderOptions obj)
      `append` ";}"

simplifyOptions :: Map Text Option -> Map Text Option
simplifyOptions = M.map simplifyOption

renderOptions :: Map Text Option -> [Text]
renderOptions obj =
  prepend
    "{"
    ( concatMap
        (\(k, o) -> prepend (T.pack (show k <> " =")) (renderOption o) `append` ";")
        (M.toList obj)
    )
    `append` "}"

indent :: [Text] -> [Text]
indent = map ("  " <>)

prepend :: Text -> [Text] -> [Text]
prepend t = \case
  [u] -> [t <> u]
  u -> t : indent u

append :: [Text] -> Text -> [Text]
append ts u = case ts of
  [t] -> [t <> u]
  _ -> ts ++ [u]

parens :: [Text] -> [Text]
parens = prepend "(" . (`append` ")")
