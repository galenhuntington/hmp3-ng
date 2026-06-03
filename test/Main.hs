module Main (main) where

import Test.Tasty

import qualified ConfigSpec
import qualified CoreSpec
import qualified LexerSpec
import qualified StyleSpec
import qualified TreeSpec
import qualified WidthSpec

main :: IO ()
main = defaultMain $ testGroup "hmp3-ng"
    [ ConfigSpec.tests
    , CoreSpec.tests
    , LexerSpec.tests
    , StyleSpec.tests
    , TreeSpec.tests
    , WidthSpec.tests
    ]

