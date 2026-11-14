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

-- | Where an expression is rendered.
--
-- A later position admits fewer expressions without parentheses, so an
-- expression decides whether it needs them by comparing the position to the
-- last one it can stand in.
data Position
  = -- | Where an expression ends, so anything can stand there.
    -- A function, @let@, @if@ or @with@ extends as far to the right as it
    -- can, so it needs this position.
    PositionEnd
  | -- | Where an expression is followed by more of the expression around it,
    -- like the condition of an @if@.
    PositionInner
  | -- | The right operand of @//@, which groups to the right.
    PositionUpdateRight
  | -- | The left operand of @//@.
    PositionUpdateLeft
  | -- | The function of an application.
    PositionApplied
  | -- | An argument or a list element.
    PositionArgument
  | -- | Where an attribute is selected from.
    PositionSelected
  deriving (Eq, Ord)

renderExpression :: Expr -> Text
renderExpression = T.unlines . go PositionEnd
  where
    parensWhen b ts = if b then parens ts else ts
    go :: Position -> Expr -> [Text]
    go p = \case
      ExprNull -> ["null"]
      ExprLitBool b -> [if b then "true" else "false"]
      ExprLitString s -> [nixString s]
      ExprLitNumber s ->
        -- Nix reads a minus in a list or an argument as subtraction.
        parensWhen
          (p > PositionApplied && s < 0)
          [ case floatingOrInteger s of
              Left f -> T.pack $ show (f :: Double)
              Right i -> T.pack $ show (i :: Integer)
          ]
      ExprLitList es -> case es of
        [] -> ["[]"]
        [e] -> surround "[" "]" $ go PositionArgument e
        _ ->
          -- If there is more than one list element, put them on separate lines.
          "[" : indent (concatMap (go PositionArgument) es) ++ ["]"]
      ExprVar s -> [s]
      ExprAttrSet m | null m -> ["{ }"]
      ExprAttrSet m ->
        -- We always put "{" and "}" on separate lines.
        "{" : indent (concatMap (uncurry goBind) (M.toList m)) ++ ["}"]
      ExprAp e1 e2 ->
        parensWhen (p > PositionApplied) $
          go PositionApplied e1 `apply` go PositionArgument e2
      ExprFun args e ->
        parensWhen (p > PositionEnd) $
          surroundWith " " "{" "}:" [T.intercalate ", " args]
            ++ go PositionEnd e
      ExprWith t e ->
        parensWhen (p > PositionEnd) $
          ("with " <> t <> ";") : go PositionEnd e
      ExprLam arg e ->
        parensWhen (p > PositionEnd) $
          prependWith " " (arg <> ":") (go PositionEnd e)
      ExprLet binds e
        | null binds -> go p e
        | otherwise ->
            parensWhen (p > PositionEnd) $
              concat
                [ ["let"],
                  indent (concatMap (uncurry goBind) (M.toList binds)),
                  ["in"],
                  go PositionEnd e
                ]
      ExprIf c t e ->
        parensWhen (p > PositionEnd) $
          joinWith
            " else "
            ( joinWith
                " then "
                (prependWith " " "if" (go PositionInner c))
                (go PositionInner t)
            )
            (go PositionEnd e)
      ExprUpdate e1 e2 ->
        parensWhen (p > PositionUpdateRight) $
          joinWith " // " (go PositionUpdateLeft e1) (go PositionUpdateRight e2)
      ExprSelect e key ->
        go PositionSelected e `append` T.cons '.' (nixAttrKey key)
    goBind key e =
      prependWith " " (nixAttrKey key <> " =") $
        (`append` ";") $
          go PositionEnd e

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
