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

--
-- abstract syntax for mpg123/321 'remote control' commands, so we get
-- type safe messaging, and parsing of results
--
-- See 'README.remote' distributed with mpg321.
--

module Syntax where

import Lexers
import Utils

import Data.Char
import Data.List            ( (\\) )
import Text.PrettyPrint

------------------------------------------------------------------------
--
-- Values we may print out:

-- Loads and starts playing <file>
--
data Load = Load !FilePath
        deriving Show

instance Pretty Load where
    ppr (Load f) = text "LOAD" <+> text f

-- If '+' or '-' is specified, jumps <frames> frames forward, or backwards,
-- respectively, in the the mp3 file.  If neither is specifies, jumps to
-- absolute frame <frames> in the mp3 file.
data Jump = Jump !Integer
        deriving Show

instance Pretty Jump where
    ppr (Jump i) = text "JUMP" <+> (text . clean . show $ i)

-- Pauses the playback of the mp3 file; if already paused, restarts playback.
data Pause = Pause
        deriving Show

instance Pretty Pause where
    ppr Pause = text "PAUSE"

-- Quits mpg321.
data Quit = Quit
        deriving Show

instance Pretty Quit where
    ppr Quit = text "QUIT"

------------------------------------------------------------------------
--
-- Values we may have to read back in

-- mpg123 tagline. Output at startup.
data Tag = Tag
        deriving Show

instance Parse Tag where
    parse _ = string "@R MPG123" `action` \_ -> Just $ T Tag

-- Track info if ID fields are in the file, otherwise file name.
data File = File (Maybe FilePath)
{-
          | Id   { title  :: Maybe String 
                 , artist :: Maybe String 
                 , album  :: Maybe String 
                 , genre  :: Maybe String }
        -- ID3:What's Golden                 
        --     Jurassic 5 Power In Numbers              
        --     2002 0000178F 0000017D 00018392 00Hip Hop/Rap
-}
        deriving Show

instance Parse File where
    parse _ = string "@I " +> anyChar `plus` epsilon `action` \s -> 
        let f = dropSpace . drop 3 $ s 
        in case f of
                'I':'D':'3':':':_ -> Just . F . File $ Nothing
                _                 -> Just . F . File $ Just f

-- Outputs information about the mp3 file after loading.
-- <a>: version of the mp3 file. Currently always 1.0 with madlib, but don't 
--      depend on it, particularly if you intend portability to mpg123 as well.
--      Float/string.
-- <b>: layer: 1, 2, or 3. Integer.
-- <c>: Samplerate. Integer.
-- <d>: Mode string. String.
-- <e>: Mode extension. Integer.
-- <f>: Bytes per frame (estimate, particularly if the stream is VBR). Integer.
-- <g>: Number of channels (1 or 2, usually). Integer.
-- <h>: Is stream copyrighted? (1 or 0). Integer.
-- <i>: Is stream CRC protected? (1 or 0). Integer.
-- <j>: Emphasis. Integer.
-- <k>: Bitrate, in kbps. (i.e., 128.) Integer.
-- <l>: Extension. Integer.
data Info = Info {
                version       :: !String,
                layer         :: !Int,     -- 1,2 or 3
                sampleRate    :: !Integer,
                playMode      :: !String,
                modeExtns     :: !Integer,
                bytesPerFrame :: !Integer,
                channelCount  :: !Integer,
                copyrighted   :: !Bool,
                checksummed   :: !Bool,
                emphasis      :: !Integer,
                bitrate       :: !Integer,
                extension     :: !Integer
            }
        deriving Show

instance Parse Info where
    parse _ = string "@S " +> anyChar `plus` epsilon `action` \s ->           -- todo error handling
        let fs = split " " . drop 3 $ s
        in Just $ I $ Info { 
                  version       = fs !! 0
                , layer         = read $ fs !! 1
                , sampleRate    = read $ fs !! 2
                , playMode      = fs !! 3
                , modeExtns     = read $ fs !! 4
                , bytesPerFrame = read $ fs !! 5
                , channelCount  = read $ fs !! 6
                , copyrighted   = toEnum (read (fs !! 7))
                , checksummed   = toEnum (read (fs !! 8))
                , emphasis      = read $ fs !! 9
                , bitrate       = read $ fs !! 10
                , extension     = read $ fs !! 11
            }

-- @F <current-frame> <frames-remaining> <current-time> <time-remaining>
-- Frame decoding status updates (once per frame).
-- Current-frame and frames-remaining are integers; current-time and
-- time-remaining floating point numbers with two decimal places.
data Frame = Frame {
                currentFrame   :: !Integer,
                framesLeft     :: !Integer,
                currentTime    :: !(Integer,Integer),
                timeLeft       :: !(Integer,Integer)
             }
        deriving Show

instance Parse Frame where
    parse _ = string "@F " +> anyChar `plus` epsilon `action` \s ->           -- todo error handling
        let fs = split " " . drop 3 $ s
        in Just $ R $ Frame {
                currentFrame = read (fs !! 0),
                framesLeft   = read (fs !! 1),
                currentTime  = let [x,y] = split "." (fs !! 2) in (read x, read y),
                timeLeft     = let [x,y] = split "." (fs !! 3) in (read x, read y)
           }

-- @P {0, 1, 2}
-- Stop/pause status.
-- 0 - playing has stopped. When 'STOP' is entered, or the mp3 file is finished.
-- 1 - Playing is paused. Enter 'PAUSE' or 'P' to continue.
-- 2 - Playing has begun again.
data Status = Stopped
            | Paused
            | Playing
        deriving (Eq, Show)
                
instance Parse Status where
    parse _ = string "@P " +> anyChar `action` \s ->           -- todo error handling
        Just $ S $ case read . drop 3 $ s :: Int of
                0 -> Stopped
                1 -> Paused
                2 -> Playing
                _ -> error "Invalid Status"

------------------------------------------------------------------------

--
-- a pretty printing class
--
class Pretty a where
    ppr :: a -> Doc

--
-- and a class for parsing 
--
class Parse a where
    parse :: a -> Lexer () Msg  -- phantom

--
-- And a wrapper type 
--
data Msg = T Tag
         | F File
         | I Info
         | R Frame
         | S Status
        deriving Show

-- lexer for any mpg321 msg
parseAll :: Lexer () Msg
parseAll = parse (undefined :: Tag)
      >||< parse (undefined :: File)
      >||< parse (undefined :: Info)
      >||< parse (undefined :: Frame)
      >||< parse (undefined :: Status)

-- | The top level parser of mpg123 messages
parser :: [Char] -> Either [Error] Msg
parser cs = case err of
                [] -> case msgs of
                        [m] -> Right m
                        ms  -> Right (head ms)   -- weird
                e  -> Left e

        where (msgs,_,err) = execLexer parseAll (cs, ())

--------------------------------------------------------------
-- some lexer fragments

anyChar :: Regexp () Msg
anyChar = alt $ ['\0' .. '\255'] \\ ['\n','\r']

draw :: Pretty a => a -> String
draw = render . ppr 
