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
    nixOptionViaCodec,
    nixOptionVia,

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

-- | Produce a Nixos module type via a type's 'codec'.
nixOptionViaCodec :: forall a. (HasCodec a) => Text
nixOptionViaCodec = nixOptionVia (codec @a)

-- | Parse a Yaml 'ByteString' using a type's 'codec'.
nixOptionVia :: ValueCodec input output -> Text
nixOptionVia = T.unlines . renderOption . go
  where
    go :: ValueCodec input output -> Option
    go = \case
      NullCodec -> emptyOption
      BoolCodec mDesc ->
        Option
          { optionType = Just (OptionTypeSimple "types.bool"),
            optionDescription = mDesc
          }
      StringCodec mDesc ->
        Option
          { optionType = Just (OptionTypeSimple "types.str"),
            optionDescription = mDesc
          }
      NumberCodec mDesc mBounds ->
        Option
          { optionType = Just $ OptionTypeSimple $ case mBounds of
              Nothing -> "types.number"
              Just bounds -> case guessNumberBoundsSymbolic bounds of
                BitUInt w -> T.pack $ "types.u" <> show w -- TODO this will not exist for u7
                BitSInt w -> T.pack $ "types.s" <> show w -- TODO this will not exist for s7
                OtherNumberBounds _ _ -> "types.number", -- TODO
            optionDescription = mDesc
          }
      HashMapCodec _ -> emptyOption -- TODO
      MapCodec _ -> emptyOption -- TODO
      ValueCodec -> emptyOption -- TODO
      ArrayOfCodec mDesc c ->
        let o = go c
         in Option
              { optionType = Just (OptionTypeListOf o),
                optionDescription = mDesc
              }
      ObjectOfCodec mDesc oc ->
        Option
          { optionType = Just (OptionTypeSubmodule (goO oc)),
            optionDescription = mDesc
          } -- TODO
      EqCodec _ _ -> emptyOption -- TODO
      BimapCodec _ _ c -> go c
      EitherCodec {} -> emptyOption -- TODO
      CommentCodec _ c -> go c -- TODO: use the comment
      ReferenceCodec {} -> emptyOption -- TODO: let-binding?
    goO :: ObjectCodec input output -> Map Text Option
    goO = \case
      _ -> M.empty -- TODO

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

data OptionType
  = OptionTypeSimple !Text
  | OptionTypeListOf !Option
  | OptionTypeSubmodule !(Map Text Option)

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
              [ prepend "description = " ([d] `append` ";") | d <- maybeToList optionDescription
              ]
          ],
      ["}"]
    ]

renderOptionType :: OptionType -> [Text]
renderOptionType = \case
  OptionTypeSimple t -> [t]
  OptionTypeListOf o -> prepend "listOf (" (renderOption o) `append` ")"
  OptionTypeSubmodule _ -> [] -- TODO

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
