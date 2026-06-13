module Main (main) where

import Test.Tasty

import ConfigSpec qualified
import CoreSpec qualified
import LexerSpec qualified
import PlaylistSpec qualified
import StyleSpec qualified
import WidthSpec qualified

main :: IO ()
main = defaultMain $ testGroup "hmp3-ng"
    [ ConfigSpec.tests
    , CoreSpec.tests
    , LexerSpec.tests
    , StyleSpec.tests
    , PlaylistSpec.tests
    , WidthSpec.tests
    ]

