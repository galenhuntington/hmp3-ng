module Main (main) where

import Test.Tasty

import qualified CoreSpec
import qualified FastIOSpec
import qualified LexerSpec
import qualified TreeSpec

main :: IO ()
main = defaultMain $ testGroup "hmp3-ng"
    [ CoreSpec.tests
    , FastIOSpec.tests
    , LexerSpec.tests
    , TreeSpec.tests
    ]
