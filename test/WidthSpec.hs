module WidthSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Width (displayWidth, toMaxWidth, toWidth)
import UI (u)

-- These tests depend on wcwidth's behavior under a UTF-8 locale and on a
-- handful of codepoints whose canonical widths are well-known:
--   * ASCII chars are 1 column.
--   * Latin-Extended chars (é, ñ) are 1 column.
--   * Common CJK chars (中) are 2 columns.
--   * The horizontal ellipsis (…) is 1 column.
-- They are not deterministic under the C locale.

tests :: TestTree
tests = testGroup "Width"
    [ testGroup "displayWidth"
        [ testCase "empty"             $ displayWidth ""              @?= 0
        , testCase "ascii"             $ displayWidth "hello"         @?= 5
        , testCase "latin-extended"    $ displayWidth (u"café")       @?= 4
        , testCase "cjk doubles each"  $ displayWidth (u"中文")       @?= 4
        , testCase "mixed"             $ displayWidth (u"中a文b")     @?= 6
        ]
    , testGroup "toMaxWidth"
        [ testCase "wider than input passes through"
            $ toMaxWidth 10 "hello"    @?= "hello"
        , testCase "exactly the width passes through"
            $ toMaxWidth 5 "hello"     @?= "hello"
        , testCase "truncate ascii with ellipsis"
            $ toMaxWidth 4 "hello"     @?= "hel" <> u"…"
        , testCase "narrower truncate"
            $ toMaxWidth 2 "hello"     @?= "h"   <> u"…"
        , testCase "width one becomes a lone ellipsis"
            $ toMaxWidth 1 "hello"     @?= u"…"
        , testCase "width zero becomes empty"
            $ toMaxWidth 0 "hello"     @?= ""
        , testCase "wide char truncation respects boundaries"
            -- "中文hi" is 6 columns (2+2+1+1); toMaxWidth 4 keeps the first
            -- wide char plus two ellipses to fill the remaining columns.
            $ toMaxWidth 4 (u"中文hi") @?= u"中……"
        , testCase "wide char gives way to single ellipsis at the boundary"
            -- "中文" is 4 columns; toMaxWidth 3 keeps the first wide char
            -- (2 columns) plus one ellipsis (1 column).
            $ toMaxWidth 3 (u"中文")   @?= u"中…"
        ]
    , testGroup "toWidth"
        [ testCase "pads short ascii"
            $ toWidth 10 "hello"       @?= "hello     "
        , testCase "pad with empty input"
            $ toWidth 4 ""             @?= "    "
        , testCase "exact width unchanged"
            $ toWidth 5 "hello"        @?= "hello"
        , testCase "truncate matches toMaxWidth when over-width"
            $ toWidth 4 "hello"        @?= u"hel…"
        , testCase "pads after a wide-char content too"
            $ toWidth 5 (u"中a")       @?= u"中a  "
        ]
    ]

