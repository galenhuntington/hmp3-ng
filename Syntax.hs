-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

--
-- abstract syntax for mpg123/321 'remote control' commands, so we get
-- type safe messaging, and parsing of results
--

module Syntax where

import Base

import qualified Data.ByteString.Char8 as P

------------------------------------------------------------------------
--
-- Values we may print out:

-- Loads and starts playing <file>
--
newtype Load = Load ByteString

instance Pretty Load where
    ppr (Load f) = mconcat ["LOAD ", f]

-- If '+' or '-' is specified, jumps <frames> frames forward, or backwards,
-- respectively, in the the mp3 file.  If neither is specifies, jumps to
-- absolute frame <frames> in the mp3 file.
newtype Jump = Jump Int

instance Pretty Jump where
    ppr (Jump i) = mconcat ["JUMP ", P.pack . show $ i]

-- Pauses the playback of the mp3 file; if already paused, restarts playback.
data Pause = Pause

instance Pretty Pause where
    ppr Pause = "PAUSE"

-- Quits mpg123.
data Quit = Quit

instance Pretty Quit where
    ppr Quit = "QUIT"

------------------------------------------------------------------------
--
-- Values we may have to read back in

-- mpg123 tagline. Output at startup.
data Tag = Tag
    deriving stock (Eq, Show)

-- ID3 info
data Id3 = Id3
        { id3title  :: !ByteString
        , id3artist :: !ByteString
        , id3album  :: !ByteString
        , id3str    :: !ByteString
        }
    deriving stock (Eq, Show)

--      , year   :: Maybe ByteString
--      , genre  :: Maybe ByteString }


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

data Mode = Once | Loop | Random | Single
    deriving stock (Eq, Bounded, Enum, Show, Read)

------------------------------------------------------------------------

--
-- a pretty printing class
--
class Pretty a where
    ppr :: a -> ByteString

--
-- And a wrapper type 
--
data Msg = T {-# UNPACK #-} !Tag
         | F                !Id3
         | I {-# UNPACK #-} !Info
         | R {-# UNPACK #-} !Frame
         | S                !Status
    deriving stock (Eq, Show)

