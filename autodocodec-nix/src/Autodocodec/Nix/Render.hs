module Autodocodec.Nix.Render
  ( toNixExpressionViaCodec,
    toNixExpressionVia,
    objectToNixExpressionViaCodec,
    objectToNixExpressionVia,
    renderExpression,
  )
where

import Autodocodec
import Autodocodec.Nix.Expression as Nix
import Data.Map (Map)
import Data.Text (Text)

toNixExpressionViaCodec :: (HasCodec a) => a -> Nix.Expression
toNixExpressionViaCodec = toNixExpressionVia codec

-- | Render a value as a nix expression via the given codec.
toNixExpressionVia :: ValueCodec a void -> a -> Nix.Expression
toNixExpressionVia c = jsonValueExpression . toJSONVia c

-- | Render an object as a nix attribute set via its 'HasObjectCodec' instance
--
-- Attrset
objectToNixExpressionViaCodec :: (HasObjectCodec a) => a -> Map Text Nix.Expression
objectToNixExpressionViaCodec = objectToNixExpressionVia objectCodec

-- | Render an object as a nix attribute set via the given object codec.
--
-- Attrset
objectToNixExpressionVia :: ObjectCodec a void -> a -> Map Text Nix.Expression
objectToNixExpressionVia c = jsonObjectExpression . toJSONObjectVia c
