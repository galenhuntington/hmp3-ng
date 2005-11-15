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
import Utils hiding (split)     -- ugly to work around alex import GHC.Exts
import qualified Utils as U

import qualified Data.FastPackedString as P

import Control.Monad
import System.IO

import GHC.Base

------------------------------------------------------------------------

doP :: P.FastString -> Msg
doP s = S $! case P.head . P.drop 3 $ s of
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
          fs  = P.split ' ' . P.drop 3 $ s
          f n = (read $ P.unpack x, read $ P.unpack y) 
            where [x,y] = P.split '.' (fs !! n)

-- Outputs information about the mp3 file after loading.
doS :: P.FastString -> Msg
doS s = let fs = P.split ' ' . P.drop 3 $ s
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
                , userinfo      = (P.packAddress "mpeg "#)
                       `P.append` (clean $ fs !! 0)
                       `P.append` (P.packAddress " layer "#)
                       `P.append` (clean $ fs !! 10)
                       `P.append` (P.packAddress "kbit/s "#)
                       `P.append` (P.pack . show) ((read . P.unpack $ fs !! 2) `div` 1000 :: Int)
                       `P.append` (P.packAddress "kHz"#)
                }

-- Track info if ID fields are in the file, otherwise file name.
doI :: P.FastString -> Msg
doI s = let f = P.dropSpaceEnd . P.dropSpace . P.drop 3 $ s 
        in case P.take 4 f of
            cs | cs == p "ID3:"# -> F . File $ Nothing
               | otherwise       -> F . File $ Just f

------------------------------------------------------------------------

parser :: Handle -> IO (Either String Msg)
parser h = do
    s <- liftM P.pack $! hGetLine h
    return $ case P.take 2 s of
        t | t == p "@R"# -> Right $ T Tag
          | t == p "@I"# -> Right $ doI s
          | t == p "@S"# -> Right $ doS s
          | t == p "@F"# -> Right $ doF s
          | t == p "@P"# -> Right $ doP s
          | t == p "@E"# -> Left $ "mpg321 error: " ++ P.unpack s
          | otherwise    -> Left $ "Strange mpg321 packet: " ++ P.unpack s

------------------------------------------------------------------------

p :: Addr# -> P.FastString
p = P.packAddress
