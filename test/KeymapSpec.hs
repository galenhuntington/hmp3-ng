module KeymapSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Keymap (dropLastUTF8)
import UI (u)

tests :: TestTree
tests = testGroup "Keymap"
    [ testGroup "dropLastUtf8"
        [ testCase "ASCII"  $ dropLastUTF8 "abc"         @?= "ab"
        , testCase "French" $ dropLastUTF8 (u"été")      @?= u"ét"
        , testCase "CJK"    $ dropLastUTF8 (u"中國")     @?= u"中"
        , testCase "Emoji"  $ dropLastUTF8 (u"Yes👍")    @?= u"Yes"
        ]
    ]

