{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2008, 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

-- Lexer for mpg123 messages

module Lexer ( mpgParser, doP, doF, doS, doI, trim ) where

import Base
import Syntax (Msg(..),Status(..),Frame(..),Info(..),Id3(..),File(..),Tag(..))

import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.UTF8 as UTF8

------------------------------------------------------------------------

-- | Strip leading and trailing whitespace.
trim :: ByteString -> ByteString
trim = P.dropWhileEnd isSpace . P.dropSpace

readPS :: ByteString -> Maybe Int
readPS = fmap fst . P.readInt

doP :: ByteString -> Either (Maybe ()) Msg
doP s = case fst <$> P.uncons s of
    Just '0' -> Right $ S Stopped
    Just '1' -> Right $ S Paused
    Just '2' -> Right $ S Playing
    Just '3' -> Left Nothing -- newer mpg123 outputs for end of song; don't need
    _        -> Left (Just ())

-- Frame decoding status updates (once per frame).
doF :: ByteString -> Maybe Msg
doF s = do
    f0 : f1 : f2 : f3 : _ <- pure $ P.split ' ' s
    currentFrame <- readPS f0
    framesLeft   <- readPS f1
    currentTime  <- readMaybe $ P.unpack f2
    timeLeft     <- max 0 <$> readMaybe (P.unpack f3)
    pure $ R Frame { currentFrame , framesLeft, currentTime, timeLeft }

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
    pure $ I $ Info $ mconcat [
        "mpeg ", fs !! 0, " ", fs !! 10, "kbit/s ",
            P.pack $ show $ hz `div` 1000, "kHz"]

-- Track info if ID fields are in the file, otherwise file name.
-- 30 chars per field?
doI :: ByteString -> Msg
doI s = let f = trim s in F . File $
    if "ID3:" `P.isPrefixOf` f
        then let ttl = toId . splitUp . P.drop 4 $ f
            -- mpg123 sometimes returns null titles
            in if P.null (id3title ttl) then Left f else Right ttl
        else Left f
  where
    splitUp :: ByteString -> [ByteString]
    splitUp f | P.null f = []
              | True     = let (a, xs) = P.splitAt 30 f in a : splitUp xs

    toId :: [ByteString] -> Id3
    toId ls = Id3 (arg 0) (arg 1) (arg 2) $ mconcat $ intersperse " : "
        $ filter (not . P.null) [arg 1, arg 2, arg 0]
      where
        ls' = map normalise ls
        arg = fromMaybe "" . (ls' !?)

-- strip spaces, and if ISO-8859-1 convert to UTF-8
normalise :: ByteString -> ByteString
normalise raw =
    let bs = trim raw
    in if UTF8.replacement_char `elem` UTF8.toString bs
        then UTF8.fromString $ P.unpack bs
        else bs

------------------------------------------------------------------------

mpgParser :: ByteString -> Either (Maybe String) Msg
mpgParser line = do
    -- bad packets are generally just \n in ID3 (and not of interest anyway)
    let skip = Left Nothing

    when (P.length line < 3) skip
    let (pre, m) = P.splitAt 3 line
        at : code : sp : _ = P.unpack pre
    when (at /= '@' || sp /= ' ') skip

    -- little helpers
    let errE = first (fmap $ const $ code : " parse error")
    let errM = errE . maybe (Left $ Just ()) Right

    case code of
        'R' -> pure $ T Tag
        'I' -> pure $ doI m
        'S' -> errM $ doS m
        'F' -> errM $ doF m
        'P' -> errE $ doP m
        'E' -> Left $ Just $ P.unpack m
        _   -> skip

