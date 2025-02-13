{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-duplicate-exports #-}

module Autodocodec.Nix.Expression
  ( Expression (..),
    Expr,
    renderExpression,
    renderExpr,
    jsonValueExpression,
    jsonObjectExpression,
  )
where

import Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

-- For backward compatibility
-- {-# DEPRECATED "Use Expression instead" #-}
type Expr = Expression

data Expression
  = ExprNull
  | ExprLitBool !Bool
  | ExprLitString !Text
  | ExprLitNumber !Scientific
  | ExprLitList ![Expression]
  | ExprVar !Text
  | ExprAttrSet !(Map Text Expression)
  | ExprAp !Expression !Expression
  | ExprFun ![Text] !Expression
  | ExprWith !Text !Expression
  deriving (Show, Eq, Ord)

-- {-# DEPRECATED renderExpr "Use renderExpression instead" #-}
renderExpr :: Expr -> Text
renderExpr = renderExpression

renderExpression :: Expr -> Text
renderExpression = T.unlines . go 0
  where
    parensWhen b ts = if b then parens ts else ts
    go :: Int -> Expr -> [Text]
    go d = \case
      ExprNull -> ["null"]
      ExprLitBool b -> [if b then "true" else "false"]
      ExprLitString s -> [T.pack $ show $ T.unpack s]
      ExprLitNumber s ->
        [ case floatingOrInteger s of
            Left f -> T.pack $ show (f :: Double)
            Right i -> T.pack $ show (i :: Integer)
        ]
      ExprLitList es -> case es of
        [] -> ["[]"]
        [e] -> surround "[" "]" $ go 0 e
        _ ->
          -- If there is more than one list element, put them on separate lines.
          "[" : indent (concatMap (go 11) es) ++ ["]"]
      ExprVar s -> [s]
      ExprAttrSet m | null m -> ["{ }"]
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
      prependWith " " (key <> " =") $
        (`append` ";") $
          go 0 e

indent :: [Text] -> [Text]
indent = map ("  " <>)

prependWith :: Text -> Text -> [Text] -> [Text]
prependWith spacer t = \case
  [] -> [t]
  (u : us) -> (t <> spacer <> u) : us

append :: [Text] -> Text -> [Text]
append = appendWith T.empty

appendWith :: Text -> [Text] -> Text -> [Text]
appendWith spacer ts u = go ts
  where
    go = \case
      [] -> [u]
      [t] -> [t <> spacer <> u]
      (t : ts') -> t : go ts'

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

parens :: [Text] -> [Text]
parens = surround "(" ")"

surround :: Text -> Text -> [Text] -> [Text]
surround = surroundWith T.empty

surroundWith :: Text -> Text -> Text -> [Text] -> [Text]
surroundWith spacer open close = prependWith spacer open . (\t -> appendWith spacer t close)

jsonValueExpression :: JSON.Value -> Expression
jsonValueExpression = go
  where
    go = \case
      JSON.Null -> ExprNull
      JSON.Bool b -> ExprLitBool b
      JSON.String s -> ExprLitString s
      JSON.Number n -> ExprLitNumber n
      JSON.Array v -> ExprLitList $ map go $ V.toList v
      JSON.Object vs -> ExprAttrSet $ jsonObjectExpression vs

jsonObjectExpression :: JSON.Object -> Map Text Expression
jsonObjectExpression = M.mapKeysMonotonic Key.toText . KeyMap.toMap . KeyMap.map jsonValueExpression
