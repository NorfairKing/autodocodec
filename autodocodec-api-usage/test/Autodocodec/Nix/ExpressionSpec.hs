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
    (ExprAp (ExprFun ["a"] (ExprVar "a")) (ExprAttrSet (M.singleton "a" (ExprLitNumber 1))), JSON.toJSON (1 :: Int)),
    (ExprAp (ExprAp (ExprVar "builtins.sub") (ExprLitNumber 3)) (ExprLitNumber 1), JSON.toJSON (2 :: Int)),
    (ExprAp (ExprLam "x" (ExprVar "x")) (ExprLitNumber 1), JSON.toJSON (1 :: Int)),
    ( ExprAp
        (ExprAp (ExprLam "x" (ExprLam "y" (ExprAp (ExprAp (ExprVar "builtins.sub") (ExprVar "x")) (ExprVar "y")))) (ExprLitNumber 3))
        (ExprLitNumber 1),
      JSON.toJSON (2 :: Int)
    ),
    (ExprAp (ExprAp (ExprVar "builtins.head") (ExprLitList [ExprLam "x" (ExprVar "x")])) (ExprLitNumber 1), JSON.toJSON (1 :: Int)),
    (ExprLet M.empty (ExprLitNumber 1), JSON.toJSON (1 :: Int)),
    ( ExprLet
        (M.fromList [("a", ExprLitNumber 1), ("b", ExprAp (ExprAp (ExprVar "builtins.add") (ExprVar "a")) (ExprLitNumber 1))])
        (ExprVar "b"),
      JSON.toJSON (2 :: Int)
    ),
    (ExprIf (ExprLitBool True) (ExprLitNumber 1) (ExprLitNumber 2), JSON.toJSON (1 :: Int)),
    (ExprIf (ExprLitBool False) (ExprLitNumber 1) (ExprIf (ExprLitBool True) (ExprLitNumber 2) (ExprLitNumber 3)), JSON.toJSON (2 :: Int)),
    ( ExprAp (ExprIf (ExprLitBool True) (ExprLam "x" (ExprVar "x")) (ExprLam "x" (ExprLitNumber 2))) (ExprLitNumber 1),
      JSON.toJSON (1 :: Int)
    ),
    ( ExprIf
        (ExprAp (ExprVar "builtins.isInt") (ExprLitNumber 1))
        (ExprAttrSet (M.singleton "a" (ExprLitNumber 1)))
        (ExprAttrSet M.empty),
      JSON.object ["a" JSON..= (1 :: Int)]
    ),
    ( ExprUpdate
        (ExprUpdate (ExprAttrSet (M.singleton "a" (ExprLitNumber 1))) (ExprAttrSet (M.fromList [("a", ExprLitNumber 2), ("b", ExprLitNumber 1)])))
        (ExprAttrSet (M.singleton "b" (ExprLitNumber 3))),
      JSON.object ["a" JSON..= (2 :: Int), "b" JSON..= (3 :: Int)]
    ),
    ( ExprUpdate
        (ExprAttrSet (M.singleton "a" (ExprLitNumber 1)))
        (ExprUpdate (ExprAttrSet (M.singleton "a" (ExprLitNumber 2))) (ExprAttrSet (M.singleton "a" (ExprLitNumber 3)))),
      JSON.object ["a" JSON..= (3 :: Int)]
    ),
    ( ExprUpdate (ExprAp (ExprLam "x" (ExprVar "x")) (ExprAttrSet (M.singleton "a" (ExprLitNumber 1)))) (ExprAttrSet (M.singleton "b" (ExprLitNumber 2))),
      JSON.object ["a" JSON..= (1 :: Int), "b" JSON..= (2 :: Int)]
    ),
    (ExprSelect (ExprAp (ExprLam "x" (ExprVar "x")) (ExprAttrSet (M.singleton "a" (ExprLitNumber 1)))) "a", JSON.toJSON (1 :: Int)),
    ( ExprSelect (ExprUpdate (ExprAttrSet (M.singleton "a" (ExprLitNumber 1))) (ExprAttrSet (M.singleton "a" (ExprLitNumber 2)))) "a",
      JSON.toJSON (2 :: Int)
    ),
    (ExprSelect (ExprAttrSet (M.singleton "a b" (ExprLitNumber 1))) "a b", JSON.toJSON (1 :: Int)),
    ( ExprSelect (ExprSelect (ExprAttrSet (M.singleton "a" (ExprAttrSet (M.singleton "b" (ExprLitNumber 1))))) "a") "b",
      JSON.toJSON (1 :: Int)
    ),
    ( ExprSelect (ExprLet (M.singleton "x" (ExprAttrSet (M.singleton "a" (ExprLitNumber 1)))) (ExprVar "x")) "a",
      JSON.toJSON (1 :: Int)
    ),
    ( ExprAp (ExprSelect (ExprAttrSet (M.singleton "f" (ExprLam "x" (ExprVar "x")))) "f") (ExprLitNumber 1),
      JSON.toJSON (1 :: Int)
    )
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
