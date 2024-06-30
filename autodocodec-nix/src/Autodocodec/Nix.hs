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
    renderNixOptionViaCodec,
    renderNixOptionTypeVia,
    renderNixOptionVia,
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
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

renderNixOptionTypeViaCodec :: forall a. (HasCodec a) => Text
renderNixOptionTypeViaCodec = renderNixOptionTypeVia (codec @a)

renderNixOptionViaCodec :: forall a. (HasObjectCodec a) => Text
renderNixOptionViaCodec = renderNixOptionVia (objectCodec @a)

renderNixOptionTypeVia :: ValueCodec input output -> Text
renderNixOptionTypeVia =
  T.unlines
    . renderOptionType
    . simplifyOptionType
    . fromMaybe (OptionTypeSimple "types.anything")
    . valueCodecNixOptionType

renderNixOptionVia :: ObjectCodec input output -> Text
renderNixOptionVia =
  T.unlines
    . renderOptions
    . objectCodecNixOption

valueCodecNixOptionType :: ValueCodec input output -> Maybe OptionType
valueCodecNixOptionType = go
  where
    go :: ValueCodec input output -> Maybe OptionType
    go = \case
      NullCodec -> Nothing
      BoolCodec _ -> Just (OptionTypeSimple "types.bool")
      StringCodec _ -> Just (OptionTypeSimple "types.str")
      NumberCodec _ mBounds -> Just $ OptionTypeSimple $ case mBounds of
        Nothing -> "types.number"
        Just bounds -> case guessNumberBoundsSymbolic bounds of
          BitUInt w -> T.pack $ "types.u" <> show w -- TODO this will not exist for u7
          BitSInt w -> T.pack $ "types.s" <> show w -- TODO this will not exist for s7
          OtherNumberBounds _ _ -> "types.number" -- TODO
      HashMapCodec _ -> Nothing -- TODO
      MapCodec _ -> Nothing -- TODO
      ValueCodec -> Just (OptionTypeSimple "types.unspecified")
      ArrayOfCodec _ c -> OptionTypeListOf <$> go c
      ObjectOfCodec _ oc -> Just (OptionTypeSubmodule (objectCodecNixOption oc))
      EqCodec _ _ -> Nothing -- TODO
      BimapCodec _ _ c -> go c
      EitherCodec _ c1 c2 -> Just $ OptionTypeOneOf (map (fromMaybe (OptionTypeSimple "types.anything")) [go c1, go c2])
      CommentCodec _ c -> go c -- TODO: use the comment
      ReferenceCodec {} -> Nothing -- TODO: let-binding?

objectCodecNixOption :: ObjectCodec input output -> Map Text Option
objectCodecNixOption = go
  where
    go :: ObjectCodec input output -> Map Text Option
    go = \case
      DiscriminatedUnionCodec {} -> M.empty -- TODO
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

emptyOption :: Option
emptyOption =
  Option
    { optionType = Nothing,
      optionDescription = Nothing
    }

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
  | OptionTypeOneOf ![OptionType]
  | OptionTypeSubmodule !(Map Text Option)

simplifyOptionType :: OptionType -> OptionType
simplifyOptionType = id -- TODO

renderOptionType :: OptionType -> [Text]
renderOptionType = \case
  OptionTypeSimple t -> [t]
  OptionTypeListOf o -> prepend "listOf (" (renderOptionType o) `append` ")"
  OptionTypeOneOf os -> prepend "oneOf [" (concatMap (parens . renderOptionType) os) `append` "]"
  OptionTypeSubmodule obj ->
    prepend
      "types.submodule { options = "
      (renderOptions obj)
      `append` ";}"

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
