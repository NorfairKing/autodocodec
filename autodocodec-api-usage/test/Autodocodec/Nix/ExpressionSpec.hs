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

    it "puts a negative number in a list in parentheses" $
      renderExpression (ExprLitList [ExprLitNumber (-1)])
        `shouldBe` "[(-1)]\n"

    it "puts an application in a list of one element in parentheses" $
      renderExpression (ExprLitList [ExprAp (ExprVar "f") (ExprVar "x")])
        `shouldBe` "[(f x)]\n"

    -- The flake's expression-check asserts that Nix reads each of these files
    -- as the value in the JSON file next to it.
    it "renders strings as before" $
      pureGoldenTextFile
        "test_resources/nix/expression/strings.nix"
        (renderExpression (ExprLitList (map ExprLitString strings)))

    it "renders the strings as JSON as before" $
      pureGoldenJSONValueFile
        "test_resources/nix/expression/strings.json"
        (JSON.toJSON strings)

    it "renders expressions as before" $
      pureGoldenTextFile
        "test_resources/nix/expression/expressions.nix"
        (renderExpression (ExprLitList (map fst expressions)))

    it "renders the values of the expressions as JSON as before" $
      pureGoldenJSONValueFile
        "test_resources/nix/expression/expressions.json"
        (JSON.toJSON (map snd expressions))

  describe "renderExpr" $
    it "renders what renderExpression renders" $
      let expression =
            ExprFun
              ["lib"]
              (ExprAp (ExprVar "lib.types.listOf") (ExprLitList [ExprLitNumber 1, ExprLitBool True]))
       in renderExpr expression `shouldBe` renderExpression expression

-- | Expressions that need parentheses where they are, and the value that Nix
-- should read each of them as.
expressions :: [(Expression, JSON.Value)]
expressions =
  [ (ExprLitNumber (-1), JSON.toJSON (-1 :: Int)),
    (ExprLitList [ExprLitNumber (-1)], JSON.toJSON [-1 :: Int]),
    (ExprLitList [ExprLitNumber (-1), ExprLitNumber 2], JSON.toJSON [-1, 2 :: Int]),
    (ExprAp (ExprVar "builtins.toString") (ExprLitNumber (-1)), JSON.toJSON ("-1" :: Text)),
    (ExprLitList [ExprAp (ExprVar "builtins.toString") (ExprLitNumber 1)], JSON.toJSON ["1" :: Text]),
    (ExprWith "builtins" (ExprAp (ExprVar "toString") (ExprLitNumber 1)), JSON.toJSON ("1" :: Text)),
    (ExprAp (ExprFun ["a"] (ExprVar "a")) (ExprAttrSet (M.singleton "a" (ExprLitNumber 1))), JSON.toJSON (1 :: Int))
  ]

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
