{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Autodocodec.Nix
  ( -- * Producing a Nixos module type
    renderNixOptionTypeViaCodec,
    renderNixOptionsViaCodec,
    renderNixOptionTypeVia,
    renderNixOptionsVia,
    valueCodecNixOptionType,
    objectCodecNixOptions,
    Option (..),
    emptyOption,
    simplifyOption,
    OptionType (..),
    simplifyOptionType,
    renderOption,
    renderOptionType,
    withNixArgs,
    optionExpression,
    optionExpr,
    optionsExpression,
    optionsExpr,
    optionTypeExpression,
    optionTypeExpr,

    -- * Rendering Haskell values as Nix values
    toNixExpressionViaCodec,
    toNixExpressionVia,
    objectToNixExpressionViaCodec,
    objectToNixExpressionVia,

    -- * Expressions
    Expression (..),
    Expr,
    renderExpression,
    renderExpr,
  )
where

import Autodocodec.Nix.Expression
import Autodocodec.Nix.Options
import Autodocodec.Nix.Render
