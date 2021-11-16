module DocTest where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Autodocodec"]
