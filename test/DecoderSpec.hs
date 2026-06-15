module DecoderSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.ByteString.Char8 qualified as P

import Decoder

-- These exercise the helpers doX, 'trim', and 'normalise'.
tests :: TestTree
tests = testGroup "Lexer.mpgParser"
    [ testGroup "status (@P)"
        [ tc "@P 0"  $ Right (P Stopped)
        , tc "@P 1"  $ Right (P Paused)
        , tc "@P 2"  $ Right (P Playing)
        , tc "@P 3"  $ Left Nothing   -- end-of-song marker: ignored (no double play)
        , tc "@P 9"  $ Left Nothing   -- unknown code
        , tc "@P "   $ Left Nothing   -- no payload
        ]
    , testGroup "frame (@F)"
        [ tc "@F 123 456 12.34 56.78" $ Right (F (Frame 123 456 12.34 56.78))
        , tc "@F 0 0 0.00 -1.00"      $ Right (F (Frame 0 0 0.00 0))  -- timeLeft clamped
        , tc "@F 1 2 3.00"            $ Left Nothing   -- too few fields
        , tc "@F a b c d"             $ Left Nothing   -- non-numeric
        ]
    , testGroup "stream info (@S)"
        [ tc "@S 1.0 1 44100 stereo 0 0 2 0 0 0 128 0"
                                      $ Right (S "mpeg 1.0 128kb/s 44kHz")
        , tc "@S 1.0 1 44100"         $ Left Nothing   -- too few fields
        ]
    , testGroup "id3 (@I)" (let s = "n\195\182rmalise" in
        [ tcId3 ["Title"]
              $ Right (I (Id3 "Title" "" "" "Title"))
        , tcId3 ["Title", "Artist"]
              $ Right (I (Id3 "Title" "Artist" "" "Artist : Title"))
        , tcId3 [" Title", "  Artist"]
              $ Right (I (Id3 "Title" "Artist" "" "Artist : Title"))
        , tcId3 ["Title", "Artist", "Album"]
              $ Right (I (Id3 "Title" "Artist" "Album" "Artist : Album : Title"))
        , tcId3 ["", "Artist"]       $ Left Nothing   -- blank title: skipped
        , tc "@I song.mp3"           $ Left Nothing   -- non-ID3 @I: don't overwrite
        , tc "@I {"                  $ Left Nothing   -- grouping marker: ignored
        , tcId3 ["nörmalise"]        $ Right (I (Id3 s "" "" s))
        , tcId3 [s]                  $ Right (I (Id3 s "" "" s))
        ])
    , testGroup "tagline, errors, junk"
        [ tc "@E some failure"       $ Left (Just "some failure")
        , tc "garbage"               $ Left Nothing   -- no @ prefix
        , tc "@F"                    $ Left Nothing   -- no space after code
        , tc "@"                     $ Left Nothing
        , tc ""                      $ Left Nothing
        ]
    ]
  where
    tc line = tc' (show line) line
    tc' tag line expected = testCase tag $ mpgParser line @?= expected
    tcId3 fields = tc' (show fields) (id3 fields)

-- | Build an "@I ID3:" line from fixed-width 30-char fields, as mpg123 emits.
id3 :: [P.ByteString] -> P.ByteString
id3 fields = "@I ID3:" <> mconcat [ P.take 30 (f <> P.replicate 30 ' ') | f <- fields ]

