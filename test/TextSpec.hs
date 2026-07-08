module TextSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Text

-- These tests depend on wcwidth's behavior under a UTF-8 locale and on a
-- handful of codepoints whose canonical widths are well-known:
--   * ASCII chars are 1 column.
--   * Latin-Extended chars (é, ñ) are 1 column.
--   * Common CJK chars (中) are 2 columns.
--   * The horizontal ellipsis (…) is 1 column.
-- They are not deterministic under the C locale.

tests :: TestTree
tests = testGroup "Text"
    [ testGroup "match"
        [ m (Just True)  "exact"         "foo"       "fooBar"
        , m (Just True)  "caseless"      "FOO"       "fooBar"
        , m Nothing      "invalid"       "["         "any[thing"
        , m (Just True)  "alt"           "(foo|az)Q" "bazQux"
        , m (Just True)  "dot"           "o.a"       "foobar"
        , m (Just True)  "Unicode"       "jör"       "Björk"
        , m (Just True)  "dot Unicode"   "j.r"       "Björk"
        , m (Just True)  "Nordic case"   "bør"       "BØrnE"
        , m (Just True)  "Greek case"    "Λω"        "ΦλΩα"
        , m (Just True)  "dot CJK"       "中.人"     "中國人"
        , m (Just False) "byte dots"     "c..te"     "côte"
        ]
    , testGroup "dropLastUTF8"
        [ testCase "ASCII"    $ dropLastUTF8 "abc"          @?= "ab"
        , testCase "empty"    $ dropLastUTF8 ""             @?= ""
        , testCase "French"   $ dropLastUTF8 (u"été")       @?= u"ét"
        , testCase "CJK"      $ dropLastUTF8 (u"中國")      @?= u"中"
        , testCase "Emoji"    $ dropLastUTF8 (u"Yes👍")     @?= u"Yes"
        , testCase "invalid"  $ dropLastUTF8 "\x80"         @?= ""
        ]
    , testGroup "trim"
        [ testCase "empty"      $ trim ""                   @?= ""
        , testCase "whitespace" $ trim "  \tfoo bar \n"     @?= "foo bar"
        ]
    , testGroup "guessEncoding"
        [ testCase "ASCII"    $ guessEncoding "abc"         @?= "abc"
        , testCase "ISO-8859" $ guessEncoding "encöde"      @?= u"encöde"
        , testCase "UTF-8"    $ guessEncoding (u"encöde")   @?= u"encöde"
        ]
    , testGroup "displayWidth"
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


m :: Maybe Bool -> String -> String -> String -> TestTree
m b tag pat str = testCase tag $ ($ u str) <$> matches (u pat) @?= b

