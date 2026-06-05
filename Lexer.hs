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

-- Outputs information about the mp3 file after loading.
doS :: ByteString -> Maybe Msg
doS s = do
    let fs = P.split ' ' s
    guard $ length fs >= 11
    hz <- readPS $ fs !! 2
    pure . I . Info $ mconcat [
        "mpeg ", fs !! 0, " ", fs !! 10, "kbit/s ",
            (P.pack . show) (hz `div` 1000 :: Int), "kHz"]

-- Track info if ID fields are in the file, otherwise file name.
-- 30 chars per field?
doI :: ByteString -> Msg
doI s = let f = trim s
        in case P.take 4 f of
            cs | cs == "ID3:" -> F . File $
                    let ttl = toId id3 . splitUp . P.drop 4 $ f
                    -- mpg123 sometimes returns null titles
                    in if P.null (id3title ttl) then Left f else Right ttl
               | otherwise    -> F . File . Left $ f
    where
        -- a default
        id3 :: Id3
        id3 = Id3 "" "" "" ""

        -- break the ID3 string up
        splitUp :: ByteString -> [ByteString]
        splitUp f
            | f == P.empty  = []
            | otherwise
            = let (a,xs) = P.splitAt 30 f   -- we expect it to be 
                  xs'    = splitUp xs
              in a : xs'

        -- and some ugly code:
        toId :: Id3 -> [ByteString] -> Id3
        toId i ls =
            let arg n = normalise $ ls !! n
                j = case length ls of
                    0   -> i

                    1   -> i { id3title  = arg 0 }

                    2   -> i { id3title  = arg 0
                             , id3artist = arg 1 }

                    _   -> i { id3title  = arg 0
                             , id3artist = arg 1
                             , id3album  = arg 2 }

            in j { id3str =
                    mconcat $ intersperse " : " $ filter (not . P.null)
                        [id3artist j, id3album j, id3title j] }

        -- strip spaces, and decide if UTF-8 or ISO-8859-1
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

    -- TODO: make doX functions total
    let errM = maybe (Left $ Just $ code : " parse error") Right
    let errE = first (fmap $ const $ code : " parse error")
    case code of
        'R' -> pure $ T Tag
        'I' -> pure $ doI m
        'S' -> errM $ doS m
        'F' -> errM $ doF m
        'P' -> errE $ doP m
        'E' -> Left $ Just $ P.unpack m
        _   -> skip

