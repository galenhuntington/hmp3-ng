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

import Data.Char
import Text.PrettyPrint

import qualified Data.FastPackedString as P

------------------------------------------------------------------------
--
-- Values we may print out:

-- Loads and starts playing <file>
--
data Load = Load !P.FastString
        deriving Show

instance Pretty Load where
    ppr (Load f) = text "LOAD" <+> (text . P.unpack $ f)  -- todo unbox

-- If '+' or '-' is specified, jumps <frames> frames forward, or backwards,
-- respectively, in the the mp3 file.  If neither is specifies, jumps to
-- absolute frame <frames> in the mp3 file.
data Jump = Jump !Int
        deriving Show

instance Pretty Jump where
    ppr (Jump i) = text "JUMP" <+> (text . show $ i)

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

-- Track info if ID fields are in the file, otherwise file name.
data File = File (Either P.FastString Id3)
        deriving Show

-- ID3 info 
data Id3 = Id3 
        { id3title  :: P.FastString 
        , id3artist :: P.FastString 
        , id3album  :: P.FastString 
        , id3str    :: P.FastString }   -- cache screen string to draw

--      , year   :: Maybe P.FastString
--      , genre  :: Maybe P.FastString }

        deriving Show


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
                userinfo      :: !P.FastString, -- user friendly string
                version       :: P.FastString,
                layer         :: Int,     -- 1,2 or 3
                sampleRate    :: Int,
                playMode      :: P.FastString,
                modeExtns     :: Int,
                bytesPerFrame :: Int,
                channelCount  :: Int,
                copyrighted   :: Bool,
                checksummed   :: Bool,
                emphasis      :: Int,
                bitrate       :: Int,
                extension     :: Int 
            }
        deriving Show

-- @F <current-frame> <frames-remaining> <current-time> <time-remaining>
-- Frame decoding status updates (once per frame).
-- Current-frame and frames-remaining are integers; current-time and
-- time-remaining floating point numbers with two decimal places.
--
-- Only 1 frame a second is actually read, so make sure it's lazy so
-- there's no need to evaluate all the others. 
-- 
data Frame = Frame {
                currentFrame   :: Int,
                framesLeft     :: Int,
                currentTime    :: !(Int,Int),
                timeLeft       :: !(Int,Int)
             }
        deriving Show

-- @P {0, 1, 2}
-- Stop/pause status.
-- 0 - playing has stopped. When 'STOP' is entered, or the mp3 file is finished.
-- 1 - Playing is paused. Enter 'PAUSE' or 'P' to continue.
-- 2 - Playing has begun again.
data Status = Stopped
            | Paused
            | Playing
        deriving (Eq, Show)
                
------------------------------------------------------------------------

--
-- a pretty printing class
--
class Pretty a where
    ppr :: a -> Doc

--
-- And a wrapper type 
--
data Msg = T Tag
         | F File
         | I Info
         | R Frame
         | S Status
        deriving Show

draw :: Pretty a => a -> String
draw = render . ppr 
