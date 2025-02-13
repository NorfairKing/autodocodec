{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Autodocodec.Nix.RenderSpec (spec) where

import Autodocodec
import Autodocodec.Nix
import Autodocodec.Usage
import qualified Data.Aeson as JSON
import Data.GenValidity.DList ()
import Data.GenValidity.DNonEmpty ()
import Data.Int
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Word
import Numeric.Natural
import Test.Syd

spec :: Spec
spec = do
  nixValueExpressionSpec @NullUnit "null" NullUnit
  nixValueExpressionSpec @Bool "bool" True
  nixValueExpressionSpec @Ordering "ordering" EQ
  nixValueExpressionSpec @Char "char" 'a'
  nixValueExpressionSpec @Text "text" "foo"
  nixValueExpressionSpec @LT.Text "lazy-text" "bar"
  nixValueExpressionSpec @String "string" "quux"
  nixValueExpressionSpec @Scientific "scientific" 6
  nixValueExpressionSpec @JSON.Object "object" mempty
  nixValueExpressionSpec @Int "int" 1
  nixValueExpressionSpec @Int8 "int8" 2
  nixValueExpressionSpec @Int16 "int16" 3
  nixValueExpressionSpec @Int32 "int32" 4
  nixValueExpressionSpec @Int64 "int64" 5
  nixValueExpressionSpec @Integer "integer" 6
  nixValueExpressionSpec @Word "word" 7
  nixValueExpressionSpec @Word8 "word8" 8
  nixValueExpressionSpec @Word16 "word16" 9
  nixValueExpressionSpec @Word32 "word32" 10
  nixValueExpressionSpec @Word64 "word64" 11
  nixValueExpressionSpec @Natural "natural" 12
  nixValueExpressionSpec @(Maybe Text) "maybe-text" Nothing
  nixValueExpressionSpec @(Maybe (Maybe Text)) "maybe-maybe-text" (Just Nothing)
  nixValueExpressionSpec @(Maybe (Maybe (Maybe Text))) "maybe-maybe-maybe-text" (Just (Just Nothing))
  nixValueExpressionSpec @(Either Bool Text) "either-bool-text" (Left True)
  nixValueExpressionSpec @(Either (Either Bool Scientific) Text) "either-either-bool-scientific-text" (Left (Left False))
  nixValueExpressionSpec @(Maybe (Either Bool Text)) "maybe-either-bool-text" (Just (Right "bar"))
  nixValueExpressionSpec @[Text] "list-text" ["hi", "ho"]
  nixObjectExpressionSpec @Via "via" (Via "one" "two")
  nixObjectExpressionSpec @These "these" (Both "a" 5)

nixValueExpressionSpec ::
  forall a.
  (HasCodec a) =>
  FilePath ->
  a ->
  Spec
nixValueExpressionSpec filePath a =
  it "outputs the same nix type as before" $
    pureGoldenTextFile
      ("test_resources/nix/expression/" <> filePath <> "-value.nix")
      (renderExpression (toNixExpressionViaCodec @a a))

nixObjectExpressionSpec ::
  forall a.
  (HasObjectCodec a) =>
  FilePath ->
  a ->
  Spec
nixObjectExpressionSpec filePath a =
  it "outputs the same nix type as before" $
    pureGoldenTextFile
      ("test_resources/nix/expression/" <> filePath <> "-object.nix")
      (renderExpression (ExprAttrSet (objectToNixExpressionViaCodec @a a)))
