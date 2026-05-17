module TreeSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Tree (doOrphans, merge)

tests :: TestTree
tests = testGroup "Tree"
    [ testGroup "doOrphans"
        [ testCase "empty"
            $ doOrphans []                       @?= []
        , testCase "bare filename"
            $ doOrphans ["song.mp3"]             @?= [(".", ["song.mp3"])]
        , testCase "single directory"
            $ doOrphans ["a/song.mp3"]           @?= [("a", ["song.mp3"])]
        , testCase "nested directory"
            $ doOrphans ["a/b/song.mp3"]         @?= [("a/b", ["song.mp3"])]
        , testCase "multiple, no merging here"
            $ doOrphans ["a/x.mp3", "a/y.mp3"]   @?= [("a", ["x.mp3"]), ("a", ["y.mp3"])]
        ]
    , testGroup "merge"
        [ testCase "empty"
            $ merge []                                           @?= []
        , testCase "singleton passes through"
            $ merge [("a", ["x"])]                               @?= [("a", ["x"])]
        , testCase "different keys are sorted"
            $ merge [("b", ["1"]), ("a", ["2"])]                 @?= [("a", ["2"]), ("b", ["1"])]
        , testCase "same key combines values"
            $ merge [("a", ["x"]), ("a", ["y"])]                 @?= [("a", ["x", "y"])]
        , testCase "preserves value order within a key"
            $ merge [("a", ["1"]), ("a", ["2"]), ("a", ["3"])]   @?= [("a", ["1", "2", "3"])]
        , testCase "value lists with multiple elements"
            $ merge [("a", ["x", "y"]), ("a", ["z"])]            @?= [("a", ["x", "y", "z"])]
        , testCase "interleaved keys"
            $ merge [("a", ["1"]), ("b", ["2"]), ("a", ["3"])]   @?= [("a", ["1", "3"]), ("b", ["2"])]
        ]
    ]
