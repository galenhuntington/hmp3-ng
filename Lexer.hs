-- 
-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2008 Galen Huntington
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
import FastIO   (FiltHandle(..), checkF, getPacket, dropSpaceEnd)
import Data.Char

import Data.Maybe   (fromJust)
import qualified Data.ByteString.Char8 as P
import Control.Monad.Except

------------------------------------------------------------------------

pSafeHead :: P.ByteString -> Char
pSafeHead s = if P.null s then ' ' else P.head s

doP :: P.ByteString -> Msg
doP s = S $! case pSafeHead s of
                '0' -> Stopped
                '1' -> Paused
                '2' -> Playing
                '3' -> Stopped  -- used by mpg321
                _ -> Playing
                -- _ -> error "Invalid Status"

-- Frame decoding status updates (once per frame).
doF :: P.ByteString -> Msg
doF s = R $ Frame {
                currentFrame = readPS (fs !! 0)
              , framesLeft   = readPS (fs !! 1)
              , currentTime  = f 2
              , timeLeft     = f 3
           }
        where

          fs  = P.split ' ' $ s
          f n = case P.split '.' (fs !! n) of { [x,y] ->
                case readPS x              of { rx    ->
                case readPS y              of { ry    -> (rx,ry) }}
                                              ; _ -> error "doF.f" }

readPS :: P.ByteString -> Int
readPS = fst . fromJust . P.readInt

-- Outputs information about the mp3 file after loading.
doS :: P.ByteString -> Msg
doS s = let fs = P.split ' ' $ s
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
                       ["mpeg "
                       ,fs !! 0
                       ," "
                       ,fs !! 10
                       ,"kbit/s "
                       ,(P.pack . show) ((readPS (fs !! 2)) `div` 1000 :: Int)
                       ,"kHz"]
                }

-- Track info if ID fields are in the file, otherwise file name.
-- 30 chars per field?
doI :: P.ByteString -> Msg
doI s = let f = dropSpaceEnd . P.dropWhile isSpace $ s
        in case P.take 4 f of
            cs | cs == "ID3:" -> F . File $
                    let ttl = toId id3 . splitUp . P.drop 4 $ f
                    -- mpg321 sometimes returns null titles
                    in if P.null (id3title ttl) then Left f else Right ttl
               | otherwise    -> F . File . Left $ f
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

        gap x y = P.concat [ x, " : ", y ]

        normalise = P.dropWhile isSpace . dropSpaceEnd

------------------------------------------------------------------------

parser :: FiltHandle -> IO (Either String Msg)
parser h = runExceptT loop where
  loop = do
    x <- lift $ getPacket h
    let -- badPacket :: ExceptT String IO a   -- MonoLocalBinds...
        badPacket = throwError $ "Bad packet: " ++ show (P.unpack x)
        -- worth notifying about? most are just newlines in the ID data

    when (P.length x < 3) $ void badPacket
    let (pre, m) = P.splitAt 3 x
        at : code : sp : _ = P.unpack pre
    when (at /= '@' || sp /= ' ') $ void badPacket

    -- TODO: make doX functions total
    case code of
        'R' -> pure $ T Tag
        'I' -> pure $ doI m
        'S' -> pure $ doS m
        'F' -> do
            b <- lift $ checkF h
            if b then pure $ doF m else loop
        'P' -> pure $ doP m
        'E' -> throwError $ "mpg321 error: " ++ P.unpack m
        _   -> badPacket

