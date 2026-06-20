module BaseSpec (tests) where

import Data.ByteString.UTF8 qualified as UTF8

import Test.Tasty
import Test.Tasty.HUnit

import Base (matches)

tests :: TestTree
tests = testGroup "Base"
    [ testGroup "match"
        [ t True "exact"         "foo"       "fooBar"
        , t True "caseless"      "FOO"       "fooBar"
        , t False "invalid"      "["         "any[thing"
        , t True "alt"           "(foo|az)Q" "bazQux"
        , t True "dot"           "o.a"       "foobar"
        , t True "Unicode"       "jör"       "Björk"
        , t True "dot Unicode"   "j.r"       "Björk"
        , t True "Nordic case"   "bør"       "BØrnE"
        , t True "Greek case"    "Λω"        "ΦλΩα"
        , t True "dot CJK"       "中.人"     "中國人"
        ]
    ]


t :: Bool -> String -> String -> String -> TestTree
t b tag pat str =
    testCase tag $ matches (UTF8.fromString pat) (UTF8.fromString str) @?= b

