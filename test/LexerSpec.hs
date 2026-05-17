module LexerSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Char8 as P

import Lexer (doP, doF, doS, doI)
import Syntax

tests :: TestTree
tests = testGroup "Lexer"
    [ testGroup "doP (status messages)"
        [ testCase "0 is Stopped"        $ status (doP "0")   @?= Stopped
        , testCase "1 is Paused"         $ status (doP "1")   @?= Paused
        , testCase "2 is Playing"        $ status (doP "2")   @?= Playing
        , testCase "3 is Playing"        $ status (doP "3")   @?= Playing
        , testCase "empty is Playing"    $ status (doP "")    @?= Playing
        , testCase "garbage is Playing"  $ status (doP "xyz") @?= Playing
        ]
    , testGroup "doF (frame messages)"
        [ testCase "all four fields parse" $ do
            let Frame{..} = frame (doF "123 456 12.34 56.78")
            currentFrame @?= 123
            framesLeft   @?= 456
            currentTime  @?= 12.34
            timeLeft     @?= 56.78
        , testCase "negative timeLeft is clamped to zero" $ do
            timeLeft (frame (doF "0 0 0.00 -1.00")) @?= 0
        ]
    , testGroup "doS (info messages)"
        [ testCase "userinfo combines version, bitrate, kHz" $
            userinfo (info (doS "1.0 1 44100 stereo 0 0 2 0 0 0 128 0"))
                @?= "mpeg 1.0 128kbit/s 44kHz"
        ]
    , testGroup "doI (track info / id3)"
        [ testCase "non-id3 input becomes Left filename" $
            fileLeft (doI "song.mp3") @?= "song.mp3"
        , testCase "non-id3 input is trimmed" $
            fileLeft (doI "  song.mp3  ") @?= "song.mp3"
        , testCase "id3 with title only" $ do
            let i = fileRight (doI ("ID3:" <> field30 "Title"))
            id3title  i @?= "Title"
            id3artist i @?= ""
            id3album  i @?= ""
            id3str    i @?= "Title"
        , testCase "id3 with title and artist" $ do
            let i = fileRight (doI ("ID3:" <> field30 "Title" <> field30 "Artist"))
            id3title  i @?= "Title"
            id3artist i @?= "Artist"
            id3album  i @?= ""
            id3str    i @?= "Artist : Title"
        , testCase "id3 with title, artist, and album" $ do
            let i = fileRight (doI ("ID3:" <> field30 "Title"
                                          <> field30 "Artist"
                                          <> field30 "Album"))
            id3title  i @?= "Title"
            id3artist i @?= "Artist"
            id3album  i @?= "Album"
            id3str    i @?= "Artist : Album : Title"
        , testCase "id3 with empty title falls back to Left of trimmed input" $
            -- mpg123 sometimes returns ID3 records with a blank title; rather than
            -- present an empty track name, the parser exposes the raw line.
            fileLeft (doI ("ID3:" <> field30 "" <> field30 "Artist"))
                @?= "ID3:" <> P.replicate 30 ' ' <> "Artist"
        ]
    ]

------------------------------------------------------------------------
-- Helpers

-- Pad/truncate a ByteString to exactly 30 characters with trailing spaces,
-- matching the fixed-width field convention used by mpg123 ID3 output.
field30 :: P.ByteString -> P.ByteString
field30 b = P.take 30 (b <> P.replicate 30 ' ')

status :: Msg -> Status
status (S s) = s
status _     = error "expected S"

frame :: Msg -> Frame
frame (R f) = f
frame _     = error "expected R"

info :: Msg -> Info
info (I i) = i
info _     = error "expected I"

fileLeft :: Msg -> P.ByteString
fileLeft (F (File (Left  s))) = s
fileLeft _                    = error "expected F (File (Left _))"

fileRight :: Msg -> Id3
fileRight (F (File (Right i))) = i
fileRight _                    = error "expected F (File (Right _))"
