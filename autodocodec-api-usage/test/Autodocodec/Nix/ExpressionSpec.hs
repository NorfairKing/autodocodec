{-# LANGUAGE OverloadedStrings #-}

module Autodocodec.Nix.ExpressionSpec (spec) where

import Autodocodec.Nix
import qualified Data.Map.Strict as M
import Test.Syd

spec :: Spec
spec = do
  describe "renderExpression" $ do
    it "renders an empty attribute set on one line" $
      renderExpression (ExprAttrSet M.empty) `shouldBe` "{ }\n"

    it "quotes an attribute key that is not a Nix identifier" $
      renderExpression (ExprAttrSet (M.singleton "foo bar" ExprNull))
        `shouldBe` "{\n  \"foo bar\" = null;\n}\n"

    it "leaves an attribute key that is a Nix identifier unquoted" $
      renderExpression (ExprAttrSet (M.singleton "foo-bar'" ExprNull))
        `shouldBe` "{\n  foo-bar' = null;\n}\n"

  describe "renderExpr" $
    it "renders what renderExpression renders" $
      let expression =
            ExprFun
              ["lib"]
              (ExprAp (ExprVar "lib.types.listOf") (ExprLitList [ExprLitNumber 1, ExprLitBool True]))
       in renderExpr expression `shouldBe` renderExpression expression
