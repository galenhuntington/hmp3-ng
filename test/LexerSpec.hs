module LexerSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Char8 as P

import Lexer (mpgParser)
import Syntax

-- 'mpgParser' is the module's pure, total entry point.  Testing through it
-- drives every doX helper via real wire lines (prefix and all), so the
-- helpers do not need to be exported.  Whitespace stripping (trim) is
-- exercised by the ID3 cases, whose fields arrive space-padded to 30.
tests :: TestTree
tests = testGroup "Lexer.mpgParser"
    [ testGroup "status (@P)"
        [ tc "@P 0"  $ Right (S Stopped)
        , tc "@P 1"  $ Right (S Paused)
        , tc "@P 2"  $ Right (S Playing)
        , tc "@P 3"  $ Left Nothing   -- end-of-song marker: ignored (no double play)
        , tc "@P 9"  $ Left Nothing   -- unknown code
        , tc "@P "   $ Left Nothing   -- no payload
        ]
    , testGroup "frame (@F)"
        [ tc "@F 123 456 12.34 56.78" $ Right (R (Frame 123 456 12.34 56.78))
        , tc "@F 0 0 0.00 -1.00"      $ Right (R (Frame 0 0 0.00 0))  -- timeLeft clamped
        , tc "@F 1 2 3.00"            $ Left Nothing   -- too few fields
        , tc "@F a b c d"             $ Left Nothing   -- non-numeric
        ]
    , testGroup "stream info (@S)"
        [ tc "@S 1.0 1 44100 stereo 0 0 2 0 0 0 128 0"
                                      $ Right (I (Info "mpeg 1.0 128kbit/s 44kHz"))
        , tc "@S 1.0 1 44100"         $ Left Nothing   -- too few fields
        ]
    , testGroup "id3 (@I)"
        [ tc (id3 ["Title"])
              $ Right (F (Id3 "Title" "" "" "Title"))
        , tc (id3 ["Title", "Artist"])
              $ Right (F (Id3 "Title" "Artist" "" "Artist : Title"))
        , tc (id3 ["Title", "Artist", "Album"])
              $ Right (F (Id3 "Title" "Artist" "Album" "Artist : Album : Title"))
        , tc (id3 ["", "Artist"])     $ Left Nothing   -- blank title: skipped
        , tc "@I song.mp3"            $ Left Nothing   -- non-ID3 @I: don't overwrite
        , tc "@I {"                   $ Left Nothing   -- grouping marker: ignored
        ]
    , testGroup "tagline, errors, junk"
        [ tc "@R a tagline"          $ Right (T Tag)
        , tc "@E some failure"       $ Left (Just "some failure")
        , tc "garbage"               $ Left Nothing   -- no @ prefix
        , tc "@F"                    $ Left Nothing   -- no space after code
        , tc "@"                     $ Left Nothing
        , tc ""                      $ Left Nothing
        ]
    ]
  where
    tc line expected = testCase (show line) $ mpgParser line @?= expected

-- | Build an "@I ID3:" line from fixed-width 30-char fields, as mpg123 emits.
id3 :: [P.ByteString] -> P.ByteString
id3 fields = "@I ID3:" <> mconcat [ P.take 30 (f <> P.replicate 30 ' ') | f <- fields ]
