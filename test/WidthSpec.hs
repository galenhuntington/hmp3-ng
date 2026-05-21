module WidthSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8

import Width (displayWidth, ellipsize, forceWidth)

-- These tests depend on wcwidth's behaviour under a UTF-8 locale and on a
-- handful of codepoints whose canonical widths are well-known:
--   * ASCII chars are 1 column.
--   * Latin-Extended chars (é, ñ) are 1 column.
--   * Common CJK chars (中) are 2 columns.
--   * The horizontal ellipsis (…) is 1 column.
-- They are not deterministic under the C locale.

tests :: TestTree
tests = testGroup "Width"
    [ testGroup "displayWidth"
        [ testCase "empty"             $ displayWidth ""                  @?= 0
        , testCase "ascii"             $ displayWidth "hello"             @?= 5
        , testCase "latin-extended"    $ displayWidth (utf8 "café")       @?= 4
        , testCase "cjk doubles each"  $ displayWidth (utf8 "中文")        @?= 4
        , testCase "mixed"             $ displayWidth (utf8 "中a文b")      @?= 6
        ]
    , testGroup "ellipsize"
        [ testCase "wider than input passes through"
            $ ellipsize 10 "hello"        @?= "hello"
        , testCase "exactly the width passes through"
            $ ellipsize 5 "hello"         @?= "hello"
        , testCase "truncate ascii with ellipsis"
            $ ellipsize 4 "hello"         @?= "hel" <> utf8 "…"
        , testCase "narrower truncate"
            $ ellipsize 2 "hello"         @?= "h"   <> utf8 "…"
        , testCase "width one becomes a lone ellipsis"
            $ ellipsize 1 "hello"         @?= utf8 "…"
        , testCase "width zero becomes empty"
            $ ellipsize 0 "hello"         @?= ""
        , testCase "wide char truncation respects boundaries"
            -- "中文hi" is 6 columns (2+2+1+1); ellipsize 4 keeps the first
            -- wide char plus two ellipses to fill the remaining columns.
            $ ellipsize 4 (utf8 "中文hi")  @?= utf8 "中……"
        , testCase "wide char gives way to single ellipsis at the boundary"
            -- "中文" is 4 columns; ellipsize 3 keeps the first wide char
            -- (2 columns) plus one ellipsis (1 column).
            $ ellipsize 3 (utf8 "中文")    @?= utf8 "中…"
        ]
    , testGroup "forceWidth"
        [ testCase "pads short ascii"
            $ forceWidth 10 "hello"       @?= "hello     "
        , testCase "pad with empty input"
            $ forceWidth 4 ""             @?= "    "
        , testCase "exact width unchanged"
            $ forceWidth 5 "hello"        @?= "hello"
        , testCase "truncate matches ellipsize when over-width"
            $ forceWidth 4 "hello"        @?= "hel" <> utf8 "…"
        , testCase "pads after a wide-char content too"
            $ forceWidth 5 (utf8 "中a")   @?= utf8 "中a" <> "  "
        ]
    ]

utf8 :: String -> ByteString
utf8 = UTF8.fromString
