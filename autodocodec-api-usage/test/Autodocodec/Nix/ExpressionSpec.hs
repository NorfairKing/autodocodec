{-# LANGUAGE OverloadedStrings #-}

module Autodocodec.Nix.ExpressionSpec (spec) where

import Autodocodec.Nix
import qualified Data.Aeson as JSON
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Test.Syd
import Test.Syd.Aeson

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

    it "escapes the start of an interpolation in a string" $
      renderExpression (ExprLitString "${foo}")
        `shouldBe` "\"\\${foo}\"\n"

    it "keeps a non-ASCII character in a string as it is" $
      renderExpression (ExprLitString "é")
        `shouldBe` "\"é\"\n"

    it "escapes the start of an interpolation in an attribute key" $
      renderExpression (ExprAttrSet (M.singleton "${foo}" ExprNull))
        `shouldBe` "{\n  \"\\${foo}\" = null;\n}\n"

    -- The flake's string-escaping-check asserts that Nix reads this file as
    -- the strings in the JSON file next to it.
    it "renders strings as before" $
      pureGoldenTextFile
        "test_resources/nix/expression/strings.nix"
        (renderExpression (ExprLitList (map ExprLitString strings)))

    it "renders the strings as JSON as before" $
      pureGoldenJSONValueFile
        "test_resources/nix/expression/strings.json"
        (JSON.toJSON strings)

  describe "renderExpr" $
    it "renders what renderExpression renders" $
      let expression =
            ExprFun
              ["lib"]
              (ExprAp (ExprVar "lib.types.listOf") (ExprLitList [ExprLitNumber 1, ExprLitBool True]))
       in renderExpr expression `shouldBe` renderExpression expression

strings :: [Text]
strings =
  [ "",
    "plain",
    "\"quoted\"",
    "back\\slash",
    "${interpolation}",
    "\\${escaped interpolation}",
    "$${double dollar}",
    "dollar at the end $",
    "dollar $ and brace {",
    "new\nline",
    "carriage\rreturn",
    "tab\tulator",
    "non-ASCII: é ü 漢字 🎉",
    "control \1 character"
  ]
