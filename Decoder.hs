-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2008, 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

-- Wire protocol for mpg123

module Decoder (
    mpgParser, Cmd(..), cmdToBS,
    Msg(..), Id3(..), Status(..), Frame(..), Info(..),
) where

import Base

import Data.ByteString.Char8 qualified as P
import Data.ByteString.UTF8 qualified as UTF8

------------------------------------------------------------------------
-- Send commands to mpg123

data Cmd = Load ByteString | Jump Int | Pause | Quit

cmdToBS :: Cmd -> ByteString
cmdToBS = \case
    Load f -> mconcat ["L ", f]
    -- Absolute or relative (+/-; not used here).
    Jump i -> mconcat ["J ", P.pack . show $ i]
    Pause  -> "P"  -- (un)pauses
    Quit   -> "Q"

------------------------------------------------------------------------
-- Receive messages from mpg123

data Msg = I                !Id3
         | S {-# UNPACK #-} !Info
         | F {-# UNPACK #-} !Frame
         | P                !Status
    deriving stock (Eq, Show)

-- ID3 info
data Id3 = Id3
        { id3title  :: !ByteString
        , id3artist :: !ByteString
        , id3album  :: !ByteString
        , id3str    :: !ByteString
--      , year   :: Maybe ByteString
--      , genre  :: Maybe ByteString }
        }
    deriving stock (Eq, Show)

-- mp3 file info; TODO maybe don't need this newtype at all?
newtype Info = Info { userinfo :: ByteString }
    deriving stock (Eq, Show)

-- Frame decoding status updates (once per frame).
-- Current-frame and frames-remaining are integers; current-time and
-- time-remaining floating point numbers with two decimal places.
data Frame = Frame {
    currentFrame   :: !Int,
    framesLeft     :: !Int,
    currentTime    :: !(Fixed E2),
    timeLeft       :: !(Fixed E2)
    }
    deriving stock (Eq, Show)

-- Stop/pause status.
data Status = Stopped | Paused | Playing
    deriving stock (Eq, Show)

-- | Strip leading and trailing whitespace.
trim :: ByteString -> ByteString
trim = P.dropWhileEnd isSpace . P.dropSpace

readPS :: ByteString -> Maybe Int
readPS = fmap fst . P.readInt

doP :: ByteString -> Maybe Msg
doP s = do
    (p, _) <- P.uncons s
    case p of
        '0' -> pure $ P Stopped
        '1' -> pure $ P Paused
        '2' -> pure $ P Playing
        -- recent mpg123 outputs 3 for end of song; don't need
        _   -> Nothing

-- Frame decoding status updates (once per frame).
doF :: ByteString -> Maybe Msg
doF s = do
    f0 : f1 : f2 : f3 : _ <- pure $ P.split ' ' s
    currentFrame <- readPS f0
    framesLeft   <- readPS f1
    currentTime  <- readMaybe $ P.unpack f2
    timeLeft     <- max 0 <$> readMaybe (P.unpack f3)
    pure $ F Frame { currentFrame, framesLeft, currentTime, timeLeft }

-- Info about mp3 file after loading.
-- Breakdown from mpg123 README.remote (as numbers):
--   0 = mpeg type (string)
--   1 = layer (int)
--   2 = sampling frequency (int)
--   3 = mode (string)
--   4 = mode extension (int)
--   5 = framesize (int)
--   6 = stereo (int)
--   7 = copyright (int)
--   8 = error protection (int)
--   9 = emphasis (int)
--  10 = bitrate (int)
--  11 = extension (int)
doS :: ByteString -> Maybe Msg
doS s = do
    let fs = P.split ' ' s
    guard $ length fs >= 11
    hz <- readPS $ fs !! 2
    pure $ S $ Info $ mconcat [
        "mpeg ", fs !! 0, " ", fs !! 10, "kb/s ",
            P.pack $ show $ hz `div` 1000, "kHz"]

-- Track info if ID fields are in the file, otherwise file name.
doI :: ByteString -> Maybe Msg
doI s = I <$> do
    ("ID3:", info) <- pure $ P.splitAt 4 s
    let id3 = parseId3 info
    guard $ not $ P.null $ id3title id3 -- title sometimes empty
    pure id3

-- Format: title (30), author (30), album (30), year (4), comment (30), genre
-- We currently only use the first three.
parseId3 :: ByteString -> Id3
parseId3 = toId . cut where
    cut f | P.null f = []
          | True     = let (a, xs) = P.splitAt 30 f in normalise a : cut xs
    toId ls = Id3 (arg 0) (arg 1) (arg 2) $ mconcat $ intersperse " : "
        $ filter (not . P.null) [arg 1, arg 2, arg 0]
      where arg = fromMaybe "" . (ls !?)

-- | Strip spaces, and if seeming ISO-8859-1 convert to UTF-8
normalise :: ByteString -> ByteString
normalise raw =
    let bs = trim raw
    in if UTF8.replacement_char `elem` UTF8.toString bs
        then UTF8.fromString $ P.unpack bs
        else bs

-- Parse line; on failure, return Just only if error to report.
mpgParser :: ByteString -> Either (Maybe String) Msg
mpgParser line = do
    -- bad packets are generally just \n in ID3 (and not of interest anyway)
    let quiet = maybe (Left Nothing) pure
    code <- quiet do
        '@' : c : ' ' : _ <- pure $ P.unpack line
        pure c
    let m = P.drop 3 line
    case code of
        'I' -> quiet $ doI m
        'S' -> quiet $ doS m
        'F' -> quiet $ doF m
        'P' -> quiet $ doP m
        'E' -> Left $ Just $ P.unpack m
        _   -> quiet Nothing

