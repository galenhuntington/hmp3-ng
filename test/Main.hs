module Main (main) where

import Test.Tasty

import CoreSpec qualified
import DecoderSpec qualified
import PlaylistSpec qualified
import StyleSpec qualified
import TextSpec qualified

main :: IO ()
main = defaultMain $ testGroup "hmp3-ng"
    [ CoreSpec.tests
    , DecoderSpec.tests
    , StyleSpec.tests
    , PlaylistSpec.tests
    , TextSpec.tests
    ]

