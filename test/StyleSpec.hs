module StyleSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Style

-- Lock in the config-string -> Color mapping and, with it, the intensity
-- assignment of each named colour (the "dark" name is normal intensity, the
-- plain name is bright).  This guards the 8-colour table against accidental
-- transcription errors.

tests :: TestTree
tests = testGroup "Style"
    [ testGroup "stringToColor"
        [ testCase "plain name is the bright hue"
            $ stringToColor "red"        @?= Just (Color Bright Red)
        , testCase "dark name is the normal hue"
            $ stringToColor "darkred"    @?= Just (Color Normal Red)
        , testCase "grey is bright black"
            $ stringToColor "grey"       @?= Just (Color Bright Black)
        , testCase "brightwhite is bright white"
            $ stringToColor "brightwhite" @?= Just (Color Bright White)
        , testCase "brown is normal yellow"
            $ stringToColor "brown"      @?= Just (Color Normal Yellow)
        , testCase "case-insensitive"
            $ stringToColor "ReD"        @?= Just red
        , testCase "default"
            $ stringToColor "default"    @?= Just Default
        , testCase "reverse"
            $ stringToColor "reverse"    @?= Just Reverse
        , testCase "unknown name"
            $ stringToColor "chartreuse" @?= Nothing
        ]
    ]
