module LexerSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Char8 as P

import Lexer (doP, doF, doS, doI, trim)
import Syntax

tests :: TestTree
tests = testGroup "Lexer"
    [ testGroup "doP (status messages)"
        [ testCase "0 is Stopped"        $ doP "0"   @?= S Stopped
        , testCase "1 is Paused"         $ doP "1"   @?= S Paused
        , testCase "2 is Playing"        $ doP "2"   @?= S Playing
        , testCase "3 is Playing"        $ doP "3"   @?= S Playing
        , testCase "empty is Playing"    $ doP ""    @?= S Playing
        , testCase "garbage is Playing"  $ doP "xyz" @?= S Playing
        ]
    , testGroup "doF (frame messages)"
        [ testCase "all four fields parse" $
            doF "123 456 12.34 56.78" @?= R (Frame 123 456 12.34 56.78)
        , testCase "negative timeLeft is clamped to zero" $
            doF "0 0 0.00 -1.00"      @?= R (Frame 0 0 0.00 0)
        ]
    , testGroup "doS (info messages)"
        [ testCase "userinfo combines version, bitrate, kHz" $
            doS "1.0 1 44100 stereo 0 0 2 0 0 0 128 0"
                @?= I (Info "mpeg 1.0 128kbit/s 44kHz")
        ]
    , testGroup "doI (track info / id3)"
        [ testCase "non-id3 input becomes Left filename" $
            doI "song.mp3"
                @?= F (File (Left "song.mp3"))
        , testCase "non-id3 input is trimmed" $
            doI "  song.mp3  "
                @?= F (File (Left "song.mp3"))
        , testCase "id3 with title only" $
            doI ("ID3:" <> field30 "Title")
                @?= F (File (Right (Id3 "Title" "" "" "Title")))
        , testCase "id3 with title and artist" $
            doI ("ID3:" <> field30 "Title" <> field30 "Artist")
                @?= F (File (Right (Id3 "Title" "Artist" "" "Artist : Title")))
        , testCase "id3 with title, artist, and album" $
            doI ("ID3:" <> field30 "Title" <> field30 "Artist" <> field30 "Album")
                @?= F (File (Right (Id3 "Title" "Artist" "Album" "Artist : Album : Title")))
        , testCase "id3 with empty title falls back to Left of trimmed input" $
            -- mpg123 sometimes returns ID3 records with a blank title; rather than
            -- present an empty track name, the parser exposes the raw line.
            doI ("ID3:" <> field30 "" <> field30 "Artist")
                @?= F (File (Left ("ID3:" <> P.replicate 30 ' ' <> "Artist")))
        ]
    , testGroup "trim"
        [ testCase "whitespace on ends" $ trim " \t foo bar \n " @?= "foo bar"
        , testCase "whitespace only"    $ trim "   "             @?= ""
        , testCase "empty"              $ trim ""                @?= ""
        ]
    ]

-- Pad/truncate a ByteString to exactly 30 characters with trailing spaces,
-- matching the fixed-width field convention used by mpg123 ID3 output.
field30 :: P.ByteString -> P.ByteString
field30 b = P.take 30 (b <> P.replicate 30 ' ')
