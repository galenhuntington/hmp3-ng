module BaseSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Base (matches)
import UI (u)

tests :: TestTree
tests = testGroup "Base"
    [ testGroup "match"
        [ m True "exact"         "foo"       "fooBar"
        , m True "caseless"      "FOO"       "fooBar"
        , m False "invalid"      "["         "any[thing"
        , m True "alt"           "(foo|az)Q" "bazQux"
        , m True "dot"           "o.a"       "foobar"
        , m True "Unicode"       "jör"       "Björk"
        , m True "dot Unicode"   "j.r"       "Björk"
        , m True "Nordic case"   "bør"       "BØrnE"
        , m True "Greek case"    "Λω"        "ΦλΩα"
        , m True "dot CJK"       "中.人"     "中國人"
        ]
    ]


m :: Bool -> String -> String -> String -> TestTree
m b tag pat str = testCase tag $ matches (u pat) (u str) @?= b

