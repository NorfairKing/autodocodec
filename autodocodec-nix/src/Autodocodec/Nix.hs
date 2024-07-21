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
    optionExpr,
    optionsExpr,
    optionTypeExpr,
    renderExpr,

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
  renderOptionType
    . simplifyOptionType
    . fromMaybe (OptionTypeSimple "lib.types.anything")
    . valueCodecNixOptionType

renderNixOptionsVia :: ObjectCodec input output -> Text
renderNixOptionsVia =
  renderOptions
    . simplifyOptions
    . objectCodecNixOption

valueCodecNixOptionType :: ValueCodec input output -> Maybe OptionType
valueCodecNixOptionType = go
  where
    mTyp = fromMaybe $ OptionTypeSimple "lib.types.anything"
    go :: ValueCodec input output -> Maybe OptionType
    go = \case
      NullCodec -> Nothing
      BoolCodec _ -> Just $ OptionTypeSimple "lib.types.bool"
      StringCodec _ -> Just $ OptionTypeSimple "lib.types.str"
      NumberCodec _ mBounds -> Just $ OptionTypeSimple $ case mBounds of
        Nothing -> "lib.types.number"
        Just bounds -> case guessNumberBoundsSymbolic bounds of
          BitUInt w -> T.pack $ "lib.types.u" <> show w -- TODO this will not exist for u7
          BitSInt w -> T.pack $ "lib.types.s" <> show w -- TODO this will not exist for s7
          OtherNumberBounds _ _ -> "lib.types.number" -- TODO
      HashMapCodec c -> Just $ OptionTypeAttrsOf $ mTyp $ go c
      MapCodec c -> Just $ OptionTypeAttrsOf $ mTyp $ go c
      ValueCodec -> Just (OptionTypeSimple "lib.types.unspecified")
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
                  { optionType = Just $ OptionTypeOneOf $ map (fromMaybe (OptionTypeSimple "lib.types.anything") . optionType) [t1, t2],
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

simplifyOptions :: Map Text Option -> Map Text Option
simplifyOptions = M.map simplifyOption

renderOption :: Option -> Text
renderOption = renderExpr . withNixArgs . optionExpr

renderOptions :: Map Text Option -> Text
renderOptions = renderExpr . withNixArgs . optionsExpr

renderOptionType :: OptionType -> Text
renderOptionType = renderExpr . withNixArgs . optionTypeExpr

withNixArgs :: Expr -> Expr
withNixArgs = ExprFun ["lib"]

optionExpr :: Option -> Expr
optionExpr Option {..} =
  ExprAp
    (ExprVar "mkOption")
    ( ExprAttrSet $
        M.fromList $
          concat
            [ [("type", optionTypeExpr typ) | typ <- maybeToList optionType],
              [("description", ExprLitString $ T.unpack d) | d <- maybeToList optionDescription]
            ]
    )

optionsExpr :: Map Text Option -> Expr
optionsExpr = ExprAttrSet . M.map optionExpr

optionTypeExpr :: OptionType -> Expr
optionTypeExpr = go
  where
    go = \case
      OptionTypeSimple s -> ExprVar s
      OptionTypeListOf ot ->
        ExprAp
          (ExprVar "lib.types.listOf")
          (go ot)
      OptionTypeAttrsOf ot ->
        ExprAp
          (ExprVar "lib.types.attrsOf")
          (go ot)
      OptionTypeOneOf os ->
        ExprAp
          (ExprVar "lib.types.oneOf")
          (ExprLitList (map go os))
      OptionTypeSubmodule os ->
        ExprAp
          (ExprVar "lib.types.submodule")
          (ExprAttrSet (M.singleton "options" (optionsExpr os)))

data Expr
  = ExprLitString !String
  | ExprLitList [Expr]
  | ExprVar !Text
  | ExprAttrSet (Map Text Expr)
  | ExprAp !Expr !Expr
  | ExprFun ![Text] !Expr
  | ExprWith !Text !Expr

renderExpr :: Expr -> Text
renderExpr = T.unlines . go 0
  where
    parensWhen b ts = if b then parens ts else ts
    go :: Int -> Expr -> [Text]
    go d = \case
      ExprLitString s -> [T.pack $ show s]
      ExprLitList es -> case es of
        [] -> ["[]"]
        [e] -> surround "[" "]" $ go 0 e
        _ ->
          -- If there is more than one list element, put them on separate lines.
          "[" : indent (concatMap (go 11) es) ++ ["]"]
      ExprVar s -> [s]
      ExprAttrSet m ->
        -- We always put "{" and "}" on separate lines.
        "{" : indent (concatMap (uncurry goBind) (M.toList m)) ++ ["}"]
      ExprAp e1 e2 ->
        parensWhen (d > 10) $
          go 11 e1 `apply` go 11 e2
      ExprFun args e ->
        parensWhen (d > 10) $
          surroundWith " " "{" "}:" [T.intercalate ", " args]
            ++ go 0 e
      ExprWith t e ->
        parensWhen (d > 10) $
          ("with " <> t <> ";") : go 0 e
    goBind key e =
      surroundWith " " (key <> " =") ";" $
        go 0 e

indent :: [Text] -> [Text]
indent = map ("  " <>)

prepend :: Text -> [Text] -> [Text]
prepend = prependWith T.empty

prependWith :: Text -> Text -> [Text] -> [Text]
prependWith spacer t = \case
  [] -> [t]
  (u : us) -> (t <> spacer <> u) : us

apply :: [Text] -> [Text] -> [Text]
apply ts1 ts2 = case (ts1, ts2) of
  ([t1], [t2]) -> [t1 <> " " <> t2]
  ([t1], _) -> prependWith " " t1 ts2
  (_, [t2]) -> ts1 `append` t2
  _ -> go ts1
    where
      go = \case
        [] -> ts2
        [t] -> prependWith " " t ts2
        (t : ts) -> t : go ts

append :: [Text] -> Text -> [Text]
append ts u = go ts
  where
    go = \case
      [] -> [u]
      [t] -> [t <> u]
      (t : ts') -> t : go ts'

parens :: [Text] -> [Text]
parens = surround "(" ")"

surround :: Text -> Text -> [Text] -> [Text]
surround = surroundWith T.empty

surroundWith :: Text -> Text -> Text -> [Text] -> [Text]
surroundWith spacer open close = prependWith spacer open . (`append` close)