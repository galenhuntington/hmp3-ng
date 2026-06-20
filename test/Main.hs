module Main (main) where

import Test.Tasty

import BaseSpec qualified
import ConfigSpec qualified
import CoreSpec qualified
import DecoderSpec qualified
import PlaylistSpec qualified
import StyleSpec qualified
import WidthSpec qualified

main :: IO ()
main = defaultMain $ testGroup "hmp3-ng"
    [ BaseSpec.tests
    , ConfigSpec.tests
    , CoreSpec.tests
    , DecoderSpec.tests
    , StyleSpec.tests
    , PlaylistSpec.tests
    , WidthSpec.tests
    ]

