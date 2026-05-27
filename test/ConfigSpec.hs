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
        [ testT "built-in styles valid" fixedStyles True
        , testT "wrong style invalid" badT False
        ]
    ]

-- Check WHNF evaluation doesn't throw.
evalOk :: a -> IO Bool
evalOk = fmap isRight . try @SomeException . evaluate

testT :: Traversable t => String -> t UIStyle -> Bool -> TestTree
testT s m b = testCase s $ (@?= b) . and =<< traverse evalOk m

-- Test that test actually tests.
badT :: [UIStyle]
badT =
    [ defaultStyle
    , defaultStyle { window = style "badcolor" "default" }
    , defaultStyle
    ]

