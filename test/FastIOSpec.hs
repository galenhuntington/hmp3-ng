module FastIOSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import FastIO (basenameP, dirnameP, packedFileNameEndClean, trim)

tests :: TestTree
tests = testGroup "FastIO"
    [ testGroup "basenameP"
        [ testCase "no slash"           $ basenameP "foo"         @?= "foo"
        , testCase "single dir"         $ basenameP "foo/bar"     @?= "bar"
        , testCase "leading slash"      $ basenameP "/foo"        @?= "foo"
        , testCase "nested"             $ basenameP "a/b/c/d.mp3" @?= "d.mp3"
        , testCase "trailing slash"     $ basenameP "foo/"        @?= ""
        , testCase "empty"              $ basenameP ""            @?= ""
        ]
    , testGroup "dirnameP"
        [ testCase "no slash"           $ dirnameP "foo"          @?= "."
        , testCase "single dir"         $ dirnameP "foo/bar"      @?= "foo"
        , testCase "leading slash"      $ dirnameP "/foo"         @?= ""
        , testCase "nested"             $ dirnameP "a/b/c/d.mp3"  @?= "a/b/c"
        ]
    , testGroup "trim"
        [ testCase "no whitespace"      $ trim "foo"              @?= "foo"
        , testCase "leading spaces"     $ trim "   foo"           @?= "foo"
        , testCase "trailing spaces"    $ trim "foo   "           @?= "foo"
        , testCase "both"               $ trim "   foo   "        @?= "foo"
        , testCase "internal preserved" $ trim "  foo bar  "      @?= "foo bar"
        , testCase "tabs and newlines"  $ trim "\t foo \n"        @?= "foo"
        , testCase "whitespace only"    $ trim "   "              @?= ""
        , testCase "empty"              $ trim ""                 @?= ""
        ]
    , testGroup "packedFileNameEndClean"
        [ testCase "no trailing"        $ packedFileNameEndClean "foo"      @?= "foo"
        , testCase "trailing slash"     $ packedFileNameEndClean "foo/"     @?= "foo"
        , testCase "trailing backslash" $ packedFileNameEndClean "foo\\"    @?= "foo"
        , testCase "multiple trailing"  $ packedFileNameEndClean "foo/\\/"  @?= "foo"
        , testCase "internal preserved" $ packedFileNameEndClean "a/b/c/"   @?= "a/b/c"
        , testCase "empty"              $ packedFileNameEndClean ""         @?= ""
        ]
    ]
