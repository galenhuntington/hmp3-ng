-- 
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

import Syntax
import FastIO   ( getFilteredPacket )

import qualified Data.FastPackedString as P

import System.IO

import Foreign.C.Types
import Foreign.Ptr

import GHC.Base

------------------------------------------------------------------------

doP :: P.FastString -> Msg
doP s = S $! case P.head . P.drop 2 $ s of
                '0' -> Stopped
                '1' -> Paused
                '2' -> Playing
                _ -> error "Invalid Status"

-- Frame decoding status updates (once per frame).
doF :: P.FastString -> Msg
doF s = R $ Frame {
                currentFrame = read $ P.unpack (fs !! 0)
              , framesLeft   = read $ P.unpack (fs !! 1)
              , currentTime  = f 2
              , timeLeft     = f 3
           }
        where
          fs  = P.split ' ' . P.drop 2 $ s
          f n = (read $ P.unpack x, read $ P.unpack y) 
            where [x,y] = P.split '.' (fs !! n)

-- Outputs information about the mp3 file after loading.
doS :: P.FastString -> Msg
doS s = let fs = P.split ' ' . P.drop 2 $ s
        in I $ Info { 
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
                , userinfo      = (p "mpeg "#)
                       `P.append` (fs !! 0)
                       `P.append` (p " "#)
                       `P.append` (fs !! 10)
                       `P.append` (p "kbit/s "#)
                       `P.append` (P.pack . show) ((read . P.unpack $ fs !! 2) `div` 1000 :: Int)
                       `P.append` (p "kHz"#)
                }

-- Track info if ID fields are in the file, otherwise file name.
-- 30 chars per field?
doI :: P.FastString -> Msg
doI s = let f = P.dropSpaceEnd . P.dropSpace . P.drop 2 $ s 
        in case P.take 4 f of
            cs | cs == p "ID3:"# -> F . File . Right . toId id3 . splitUp . P.drop 4 $ f
               | otherwise       -> F . File . Left $ f
    where
        -- a default
        id3 :: Id3
        id3 = Id3 P.empty P.empty P.empty P.empty

        -- break the ID3 string up
        splitUp :: P.FastString -> [P.FastString]
        splitUp f
            | f == P.empty  = []
            | otherwise     
            = let (a,xs) = P.splitAt 30 f
                  xs'    = splitUp xs
              in a : xs'

        -- and some ugly code:
        toId :: Id3 -> [P.FastString] -> Id3
        toId i ls = 
            let j = case length ls of
                    0   -> i

                    1   -> i { id3title  = normalise $! ls !! 0 }

                    2   -> i { id3title  = normalise $! ls !! 0
                             , id3artist = normalise $! ls !! 1 }

                    _   -> i { id3title  = normalise $! ls !! 0
                             , id3artist = normalise $! ls !! 1
                             , id3album  = normalise $! ls !! 2 }

            in j { id3str = id3artist j `gap` id3album j `gap` id3title j  } 

        gap x y = x `P.append` (p " : "#) `P.append` y

        normalise = P.dropSpace . P.dropSpaceEnd

------------------------------------------------------------------------

--
-- | This function does the most allocations in the long run.
-- How can we discard most "@F" input?
--
parser :: Ptr CFile -> IO (Either String Msg)
parser h = do
    s' <- getFilteredPacket h
    let s = P.dropWhile (== '@') s'
    return $ case P.head s of
        'R' -> Right $ T Tag
        'I' -> Right $ doI s
        'S' -> Right $ doS s
        'F' -> Right $ doF s
        'P' -> Right $ doP s
        'E' -> Left $ "mpg321 error: " ++ P.unpack s
        _   -> Left $ "Strange mpg321 packet: " ++ P.unpack s

------------------------------------------------------------------------

p :: Addr# -> P.FastString
p = P.packAddress
