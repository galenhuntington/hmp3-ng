-- 
-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2008, 2019-2025 Galen Huntington
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

-- Lexer for mpg123 messages

module Lexer ( parser ) where

import Base

import Syntax   (Msg(..),Status(..),Frame(..),Info(..),Id3(..),File(..),Tag(..))
import FastIO   (FiltHandle(..), checkF, getPacket, dropSpaceEnd)

import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.UTF8 as UTF8
import Control.Monad.Except
import Control.Monad.Trans (lift)

------------------------------------------------------------------------

pSafeHead :: ByteString -> Char
pSafeHead s = if P.null s then ' ' else P.head s

readPS :: ByteString -> Int
readPS = fst . fromJust . P.readInt

doP :: ByteString -> Msg
doP s = S case pSafeHead s of
                '0' -> Stopped
                '1' -> Paused
                '2' -> Playing
                -- mpg123 outputs this but then @P 0 causing double Plays
                -- (I think old mpg123 didn't do @P 0 so I added this)
                -- '3' -> Stopped  -- used by mpg123 for end of song
                _ -> Playing
                -- _ -> error "Invalid Status"

-- Frame decoding status updates (once per frame).
doF :: ByteString -> Msg
doF s = R Frame {
                currentFrame = readPS f0
              , framesLeft   = readPS f1
              , currentTime  = read . P.unpack $ f2
              , timeLeft     = max 0 . read . P.unpack $ f3
           }
        where
          f0 : f1 : f2 : f3 : _ = P.split ' ' s

-- Outputs information about the mp3 file after loading.
doS :: ByteString -> Msg
doS s = let fs = P.split ' ' s
        in I Info {
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
                userinfo      = mconcat
                       ["mpeg "
                       ,fs !! 0
                       ," "
                       ,fs !! 10
                       ,"kbit/s "
                       ,(P.pack . show) (readPS (fs !! 2) `div` 1000 :: Int)
                       ,"kHz"]
                }

-- Track info if ID fields are in the file, otherwise file name.
-- 30 chars per field?
doI :: ByteString -> Msg
doI s = let f = dropSpaceEnd . P.dropWhile isSpace $ s
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
            let bs = P.dropWhile isSpace . dropSpaceEnd $ raw
            in if UTF8.replacement_char `elem` UTF8.toString bs
                then UTF8.fromString $ P.unpack bs
                else bs

------------------------------------------------------------------------

parser :: FiltHandle -> IO (Either (Maybe String) Msg)
parser h = runExceptT do
    x <- lift $ getPacket h
    -- bad packets are generally just \n in ID3 (and not of interest anyway)
    let skip = throwError Nothing

    when (P.length x < 3) skip
    let (pre, m) = P.splitAt 3 x
        at : code : sp : _ = P.unpack pre
    when (at /= '@' || sp /= ' ') skip

    -- TODO: make doX functions total
    case code of
        'R' -> pure $ T Tag
        'I' -> pure $ doI m
        'S' -> pure $ doS m
        'F' -> do
            b <- lift $ checkF h
            if b then pure $ doF m else skip
        'P' -> pure $ doP m
        'E' -> throwError $ Just $ "mpg123 error: " ++ P.unpack m
        _   -> skip

