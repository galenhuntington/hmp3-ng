-- Copyright (c) 2004-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

--
-- | The top level application state, and operations on that value.
--
module State where

import Base

import Decoder                  (Status, Mode, Frame, Info, Id3, Cmd(cmdToBS))
import Playlist                 (FileArray, DirArray)
import Style                    (StringA, UIStyle)

import Data.ByteString          (hPut)
import System.Clock             (TimeSpec(..))
import System.IO                (hFlush)
import System.Process           (ProcessHandle)
import System.Random            (StdGen)
import Text.Regex.PCRE.Light    (Regex)


-- | Player state
data HState = HState
    -- These never change
    { music           :: !FileArray
    , folders         :: !DirArray
    , size            :: !Int                  -- cache size of list
    , bootTime        :: !TimeSpec
    , configPath      :: !(Maybe FilePath)     -- style.conf override (CLI)
    -- These can
    , current         :: !Int                  -- currently playing mp3
    , cursor          :: !Int                  -- mp3 under the cursor
    , clock           :: !(Maybe Frame)        -- current clock value
    , randomGen       :: !StdGen               -- random seed
    , mpgPid          :: !(Maybe ProcessHandle) -- pid of decoder
    , spawns          :: !Integer              -- count of decoder spawns
    , threads         :: ![ThreadId]           -- all our threads
    , id3             :: !(Maybe Id3)          -- maybe mp3 id3 info
    , info            :: !(Maybe Info)         -- mp3 info
    , status          :: !Status
    , minibuffer      :: !StringA              -- contents of minibuffer
    , modal           :: !(Maybe Modal)        -- modal visible
    , miniFocused     :: !Bool                 -- is the mini buffer focused?
    , mode            :: !Mode
    , uptime          :: !ByteString
    , regex           :: !(Maybe (Regex,Bool)) -- most recent search pattern and direction
    , searchHist      :: ![String]
    , exiting         :: !Bool                 -- let mpg123 die?
    , playHist        :: !(Seq (TimeSpec, Int))
    , histSize        :: Int
    , config          :: !UIStyle
    }

-- Each is (timestamp-string, (song-index, song-name)).
type HistDisplay = [(ByteString, (Int, ByteString))]

-- (list-of-keys, description)
type KeysHelp = ([Char], ByteString)

data Modal = HelpModal ![KeysHelp] | ExitModal | HistModal !HistDisplay

-- | A global variable holding the state.
hState :: MVar HState
hState = unsafePerformIO newEmptyMVar
{-# NOINLINE hState #-}

-- | The refresh thread waits on this
modified :: MVar ()
modified = unsafePerformIO newEmptyMVar
{-# NOINLINE modified #-}

-- | Queues a refresh.
setModified :: IO ()
setModified = void $ tryPutMVar modified ()

------------------------------------------------------------------------
-- The decoder.

data Mpg = Mpg
    { writeh :: !Handle
    , readh  :: !Handle
    , errh   :: !Handle
    }

mpg :: MVar Mpg
mpg = unsafePerformIO newEmptyMVar
{-# NOINLINE mpg #-}

sendMpg :: Cmd a => a -> IO ()
sendMpg s = withMVar mpg $ (. writeh) \h ->
    hPut h (cmdToBS s) *> hPut h "\n" *> hFlush h

------------------------------------------------------------------------
-- state accessor functions

-- | Access a component of the state with a projection function
getsHS :: (HState -> a) -> IO a
getsHS f = f <$> readMVar hState

-- | Modify the state with a pure function and no refresh
silentlyModifyHS :: (HState -> HState) -> IO ()
silentlyModifyHS  f = modifyMVar_ hState (pure . f)

modifyHS_ :: (HState -> HState) -> IO ()
modifyHS_ f = silentlyModifyHS f <* setModified

-- | Modify the state returning a value
modifyHS :: (HState -> (HState, a)) -> IO a
modifyHS f = modifyMVar hState (pure . f) <* setModified

