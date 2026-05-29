module ConfigSpec (tests) where

import Control.Exception
import Data.Either (isRight)

import Test.Tasty
import Test.Tasty.HUnit

import Config (fixedStyles, defaultStyle)
import Style (UIStyle(..), style)

tests :: TestTree
tests = testGroup "Config"
    [ testGroup "styles"
        [ testStyles "built-in styles valid" fixedStyles True
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

