-- Copyright (c) 2004-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

--
-- | The top level application state, and operations on that value.
--
module State where

import Base

import Decoder                  (Status, Frame, Id3, Cmd, cmdToBS)
import Playlist                 (FileArray, DirArray)
import Style                    (StringA(Fast), UIStyle, warnings)

import Data.ByteString          (hPut)
import System.Clock             (TimeSpec(..))
import System.IO                (hFlush)
import System.Process           (ProcessHandle)
import System.Random            (StdGen)


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
    , spawns          :: !Integer              -- count of decoder spawns
    , threads         :: ![ThreadId]           -- all our threads
    , id3             :: !(Maybe Id3)          -- maybe mp3 id3 info
    , info            :: !(Maybe ByteString)   -- mp3 info
    , status          :: !Status
    , minibuffer      :: !StringA              -- contents of minibuffer
    , modal           :: !(Maybe Modal)        -- modal visible
    , miniFocused     :: !Bool                 -- is the mini buffer focused?
    , mode            :: !Mode
    , uptime          :: !ByteString
    , searchFw        :: !Bool                 -- active search direction
    , searchHist      :: ![ByteString]
    , playHist        :: !(Seq (TimeSpec, Int))
    , histSize        :: Int
    , config          :: !UIStyle
    }

data Mode = Once | Loop | Random | Single
    deriving stock (Eq, Bounded, Enum, Show, Read)

-- Each is (timestamp-string, (song-index, song-name)).
type HistDisplay = [(ByteString, (Int, ByteString))]

-- (list-of-keys, description)
type KeysHelp = ([Char], ByteString)

data Modal = HelpModal ![KeysHelp] | ExitModal | HistModal !HistDisplay

-- | A global variable holding the state.
hState :: MVar HState
hState = unsafePerformIO newEmptyMVar
{-# NOINLINE hState #-}

-- | Decoder process handle
-- If Just, handles should be somewhere.
mpgProcess :: IORef (Maybe ProcessHandle)
mpgProcess = unsafePerformIO $ newIORef Nothing
{-# NOINLINE mpgProcess #-}

-- | The refresh thread waits on this
modified :: MVar ()
modified = unsafePerformIO newEmptyMVar
{-# NOINLINE modified #-}

-- | Queues a refresh.
setModified :: IO ()
setModified = void $ tryPutMVar modified ()

------------------------------------------------------------------------
-- The decoder.

data Mpg = Mpg { errh :: !Handle, writeh :: !Handle }

mpg :: MVar Mpg
mpg = unsafePerformIO newEmptyMVar
{-# NOINLINE mpg #-}

sendMpg :: Cmd -> IO ()
sendMpg c = do
    running <- isJust <$> readIORef mpgProcess
    if running
    then withMVar mpg $ (. writeh) \h ->
        hPut h (cmdToBS c) *> hPut h "\n" *> hFlush h
    else
        modifyHS_ \st -> st { minibuffer =
            case minibuffer st of -- don't overwrite if message already
                Fast "" _ -> Fast "mpg123 process not running" (warnings $ config st)
                _         -> minibuffer st
        }

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

