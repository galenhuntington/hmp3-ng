-- 
-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- 
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
-- 

-- Lexer for mpg321 messages

module Lexer ( parser ) where

import Syntax   (Msg(..),Status(..),Frame(..),Info(..),Id3(..),File(..),Tag(..))
import FastIO   (getFilteredPacket, dropSpaceEnd)
import Data.Char

import Data.Maybe   (fromJust)
import qualified Data.ByteString.Char8 as P

import Foreign.C.Types  (CFile)
import Foreign.Ptr      (Ptr)

------------------------------------------------------------------------

doP :: P.ByteString -> Msg
doP s = S $! case P.head . P.tail $ s of
                '0' -> Stopped
                '1' -> Paused
                '2' -> Playing
                _ -> error "Invalid Status"

-- Frame decoding status updates (once per frame).
doF :: P.ByteString -> Msg
doF s = R $ Frame {
                currentFrame = readPS (fs !! 0)
              , framesLeft   = readPS (fs !! 1)
              , currentTime  = f 2
              , timeLeft     = f 3
           }
        where

          fs  = P.split ' ' . P.tail $ s
          f n = case P.split '.' (fs !! n) of { [x,y] ->
                case readPS x              of { rx    ->
                case readPS y              of { ry    -> (rx,ry) }}
                                              ; _ -> error "doF.f" }

readPS :: P.ByteString -> Int
readPS = fst . fromJust . P.readInt

-- Outputs information about the mp3 file after loading.
doS :: P.ByteString -> Msg
doS s = let fs = P.split ' ' . P.tail $ s
        in I $ Info {
            {-
                  version       = fs !! 0
                , layer         = read . P.unpack $ fs !! 1
                , sampleRate    = read . P.unpack $ fs !! 2
                , playMode      = fs !! 3
                , modeExtns     = read . P.unpack $ fs !! 4
                , bytesPerFrame = read . P.unpack $ fs !! 5
                , channelCount  = read . P.unpack $ fs !! 6
                , copyrighted   = toEnum (read $ P.unpack (fs !! 7))
                , checksummed   = toEnum (read $ P.unpack (fs !! 8))
                , emphasis      = read $ P.unpack $ fs !! 9
                , bitrate       = read $ P.unpack $ fs !! 10
                , extension     = read $ P.unpack $ fs !! 11
            -}
                userinfo      = P.concat
                       [P.pack "mpeg "
                       ,fs !! 0
                       ,P.pack " "
                       ,fs !! 10
                       ,P.pack "kbit/s "
                       ,(P.pack . show) ((readPS (fs !! 2)) `div` 1000 :: Int)
                       ,P.pack "kHz"]
                }

-- Track info if ID fields are in the file, otherwise file name.
-- 30 chars per field?
doI :: P.ByteString -> Msg
doI s = let f = dropSpaceEnd . P.dropWhile isSpace . P.tail $ s 
        in case P.take 4 f of
            cs | cs == P.pack "ID3:" -> F . File . Right . toId id3 . splitUp . P.drop 4 $ f
               | otherwise           -> F . File . Left $ f
    where
        -- a default
        id3 :: Id3
        id3 = Id3 P.empty P.empty P.empty P.empty

        -- break the ID3 string up
        splitUp :: P.ByteString -> [P.ByteString]
        splitUp f
            | f == P.empty  = []
            | otherwise
            = let (a,xs) = P.splitAt 30 f   -- we expect it to be 
                  xs'    = splitUp xs
              in a : xs'

        -- and some ugly code:
        toId :: Id3 -> [P.ByteString] -> Id3
        toId i ls =
            let j = case length ls of
                    0   -> i

                    1   -> i { id3title  = normalise $! ls !! 0 }

                    2   -> i { id3title  = normalise $! ls !! 0
                             , id3artist = normalise $! ls !! 1 }

                    _   -> i { id3title  = normalise $! ls !! 0
                             , id3artist = normalise $! ls !! 1
                             , id3album  = normalise $! ls !! 2 }

            in j { id3str = (id3artist j)
                        `maybeJoin`
                            (id3album j)
                        `maybeJoin`
                            (id3title j) }

        maybeJoin t f = if P.null f then t `P.append` P.empty else t `gap` f

        gap x y = P.concat [ x, (P.pack " : "), y ]

        normalise = P.dropWhile isSpace . dropSpaceEnd

------------------------------------------------------------------------

--
-- | This function does the most allocations in the long run.
-- How can we discard most "\@F" input?
--
parser :: Ptr CFile -> IO (Either String Msg)
parser h = do
    x  <- getFilteredPacket h

    -- normalise the packet
    let (s,m) = let a    = P.dropWhile (== ' ') x       -- drop any leading whitespace
                    (b,d)= P.break (== ' ') a           -- split into header and body
                    b'   = if '@' `P.elem` b
                           then P.dropWhile (/= '@') b -- make sure '@' is first char
                           else b
                in (P.dropWhile (== '@') b', d)

    return $ case P.head s of
        'R' -> Right $ T Tag
        'I' -> Right $ doI m
        'S' -> Right $ doS m
        'F' -> Right $ doF m
        'P' -> Right $ doP m
        'E' -> Left $ "mpg321 error: " ++ P.unpack x
        _   -> Left $ "Strange mpg321 packet: " ++ (show (P.unpack x))

