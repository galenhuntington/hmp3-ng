module StyleSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception

import Base (isRight)
import Style

-- Lock in the config-string -> Color mapping and, with it, the intensity
-- assignment of each named color (the "dark" name is normal intensity, the
-- plain name is bright).  This guards the 8-color table against accidental
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
            $ stringToColor "ReD"        @?= Just (Color Bright Red)
        , testCase "default"
            $ stringToColor "default"    @?= Just Default
        , testCase "reverse"
            $ stringToColor "reverse"    @?= Just Reverse
        , testCase "unknown name"
            $ stringToColor "chartreuse" @?= Nothing
        ]
    , testGroup "styles"
        [ testStyles "built-in styles valid" [defaultStyle, monoStyle] True
        , testStyles "wrong style invalid" badStyles False
        ]
    ]

-- Check WHNF evaluation doesn't throw.
-- Suffices because UIStyle is recursively strict.
evalOk :: a -> IO Bool
evalOk = fmap isRight . try @SomeException . evaluate

testStyles :: Traversable t => String -> t UIStyle -> Bool -> TestTree
testStyles s m b = testCase s $ (@?= b) . and =<< traverse evalOk m

-- Test that test actually tests.
badStyles :: [UIStyle]
badStyles =
    [ defaultStyle
    , defaultStyle { window = style "badcolor" "default" }
    , defaultStyle
    ]

