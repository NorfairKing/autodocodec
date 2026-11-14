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
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
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
  | -- | A function with a set pattern: @{ a, b }: e@
    ExprFun ![Text] !Expression
  | ExprWith !Text !Expression
  | -- | A function with a single argument: @a: e@
    ExprLam !Text !Expression
  | ExprLet !(Map Text Expression) !Expression
  | ExprIf !Expression !Expression !Expression
  | -- | @e1 // e2@
    ExprUpdate !Expression !Expression
  | -- | @e.key@
    ExprSelect !Expression !Text
  deriving (Show, Eq, Ord)

-- {-# DEPRECATED renderExpr "Use renderExpression instead" #-}
renderExpr :: Expr -> Text
renderExpr = renderExpression

renderExpression :: Expr -> Text
renderExpression = T.unlines . go 0
  where
    parensWhen b ts = if b then parens ts else ts
    -- The precedence of the context, from low to high: anything (0), an
    -- operand of '//' (5 and 6), a function that is applied (10), an
    -- argument or list element (11), and something an attribute is selected
    -- from (12).
    -- A function, let, if, or with extends as far to the right as it can, so
    -- it needs parentheses anywhere but where an expression ends.
    go :: Int -> Expr -> [Text]
    go d = \case
      ExprNull -> ["null"]
      ExprLitBool b -> [if b then "true" else "false"]
      ExprLitString s -> [nixString s]
      ExprLitNumber s ->
        -- Nix reads a minus in a list or an argument as subtraction.
        parensWhen
          (d > 10 && s < 0)
          [ case floatingOrInteger s of
              Left f -> T.pack $ show (f :: Double)
              Right i -> T.pack $ show (i :: Integer)
          ]
      ExprLitList es -> case es of
        [] -> ["[]"]
        [e] -> surround "[" "]" $ go 11 e
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
          go 10 e1 `apply` go 11 e2
      ExprFun args e ->
        parensWhen (d > 0) $
          surroundWith " " "{" "}:" [T.intercalate ", " args]
            ++ go 0 e
      ExprWith t e ->
        parensWhen (d > 0) $
          ("with " <> t <> ";") : go 0 e
      ExprLam arg e ->
        parensWhen (d > 0) $
          prependWith " " (arg <> ":") (go 0 e)
      ExprLet binds e
        | null binds -> go d e
        | otherwise ->
            parensWhen (d > 0) $
              concat
                [ ["let"],
                  indent (concatMap (uncurry goBind) (M.toList binds)),
                  ["in"],
                  go 0 e
                ]
      ExprIf c t e ->
        parensWhen (d > 0) $
          joinWith
            " else "
            (joinWith " then " (prependWith " " "if" (go 1 c)) (go 1 t))
            (go 0 e)
      ExprUpdate e1 e2 ->
        parensWhen (d > 5) $
          joinWith " // " (go 6 e1) (go 5 e2)
      ExprSelect e key ->
        go 12 e `append` T.cons '.' (nixAttrKey key)
    goBind key e =
      prependWith " " (nixAttrKey key <> " =") $
        (`append` ";") $
          go 0 e

-- | Quote an attribute key if it is not a valid Nix identifier.
-- Valid identifiers match @[a-zA-Z_][a-zA-Z0-9_'-]*@.
nixAttrKey :: Text -> Text
nixAttrKey key
  | isNixIdentifier key = key
  | otherwise = nixString key

-- | A Nix string literal that Nix reads as the given text.
--
-- Not Haskell's 'show', because Nix has no numeric escapes, so it reads
-- @\"\\233\"@ as @\"233\"@, and because Nix interpolates @${@.
nixString :: Text -> Text
nixString s =
  let escape :: Char -> Text
      escape = \case
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        c -> T.singleton c
   in T.concat ["\"", T.replace "${" "\\${" (T.concatMap escape s), "\""]

isNixIdentifier :: Text -> Bool
isNixIdentifier t = case T.uncons t of
  Nothing -> False
  Just (c, rest) -> isNixIdentStart c && T.all isNixIdentChar rest
  where
    isNixIdentStart c = c == '_' || isAsciiLower c || isAsciiUpper c
    isNixIdentChar c = isNixIdentStart c || isDigit c || c == '-' || c == '\''

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

-- | Join the last line of the first with the first line of the second.
joinWith :: Text -> [Text] -> [Text] -> [Text]
joinWith spacer ts1 = \case
  [] -> ts1
  (t2 : ts2) -> appendWith spacer ts1 t2 ++ ts2

apply :: [Text] -> [Text] -> [Text]
apply ts1 ts2 = case (ts1, ts2) of
  ([t1], [t2]) -> [t1 <> " " <> t2]
  ([t1], _) -> prependWith " " t1 ts2
  (_, [t2]) -> appendWith " " ts1 t2
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
