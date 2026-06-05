-- Copyright (c) 2004-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

--
-- | The top level application state, and operations on that value.
--
module State where

import Base

import Syntax                   (Status(Stopped), Mode(..), Frame, Info, Id3, Pretty(ppr))
import Tree                     (FileArray, DirArray)
import Style                    (StringA(Fast), defaultSty, UIStyle)
import qualified Config (defaultStyle)

import Text.Regex.PCRE.Light    (Regex)
import Data.Array               (listArray)
import Data.ByteString          (hPut)
import Data.Sequence            (Seq)
import System.Clock             (TimeSpec(..))
import System.IO                (hFlush)
import System.Process           (ProcessHandle)
import System.Random


-- | The editor state type
data HState = HState {
        music           :: !FileArray
       ,folders         :: !DirArray
       ,size            :: !Int                  -- cache size of list
       ,current         :: !Int                  -- currently playing mp3
       ,cursor          :: !Int                  -- mp3 under the cursor
       ,clock           :: !(Maybe Frame)        -- current clock value
       ,clockUpdate     :: !Bool
       ,randomGen       :: !StdGen               -- random seed
       ,mpgPid          :: !(Maybe ProcessHandle) -- pid of decoder
       ,spawns          :: !Integer              -- count of decoder spawns
       ,threads         :: ![ThreadId]           -- all our threads
       ,id3             :: !(Maybe Id3)          -- maybe mp3 id3 info
       ,info            :: !(Maybe Info)         -- mp3 info
       ,status          :: !Status
       ,minibuffer      :: !StringA              -- contents of minibuffer
       ,modal           :: !(Maybe Modal)        -- modal visible
       ,miniFocused     :: !Bool                 -- is the mini buffer focused?
       ,mode            :: !Mode
       ,uptime          :: !ByteString
       ,boottime        :: !TimeSpec
       ,regex           :: !(Maybe (Regex,Bool)) -- most recent search pattern and direction
       ,searchHist      :: ![String]             -- history of searches
       ,doNotResuscitate :: !Bool                -- should we just let mpg123 die?
       ,playHist        :: !(Seq (TimeSpec, Int)) -- limited history of songs played
       ,config          :: !UIStyle             -- config values
       ,configPath      :: !(Maybe FilePath)     -- style.conf override (CLI)

       ,modified        :: !(MVar ())           -- Set when redrawable components of 
                                                -- the state are modified. The ui
                                                -- refresh thread waits on this.
    }

-- Each is (timestamp-string, (song-index, song-name)).
type HistDisplay = [(ByteString, (Int, ByteString))]

-- (list-of-keys, description)
type KeysHelp = ([Char], ByteString)

data Modal = HelpModal ![KeysHelp] | ExitModal | HistModal !HistDisplay

------------------------------------------------------------------------
--
-- | The initial state
--
newEmptyHS :: IO HState
newEmptyHS = do
    modified  <- newEmptyMVar
    randomGen <- newStdGen
    pure HState {
        music        = listArray (0,0) []
       ,folders      = listArray (0,0) []

       ,size         = 0
       ,current      = 0
       ,cursor       = 0

       ,threads      = []
       ,modified

       ,mpgPid       = Nothing
       ,spawns       = 0
       ,clock        = Nothing
       ,info         = Nothing
       ,id3          = Nothing
       ,regex        = Nothing
       ,searchHist   = []

       ,clockUpdate      = False
       ,modal            = Nothing
       ,miniFocused      = False
       ,doNotResuscitate = False    -- mpg123 should be restarted

       ,randomGen
       ,playHist     = mempty
       ,config       = Config.defaultStyle
       ,configPath   = Nothing
       ,boottime     = 0
       ,status       = Stopped
       ,mode         = minBound
       ,minibuffer   = Fast mempty defaultSty
       ,uptime       = mempty
    }

--
-- | A global variable holding the state.
--
hState :: MVar HState
hState = unsafePerformIO $ newMVar =<< newEmptyHS
{-# NOINLINE hState #-}

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

sendMpg :: Pretty a => a -> IO ()
sendMpg s = withMVar mpg $ (. writeh) \h ->
    hPut h (ppr s) >> hPut h "\n" >> hFlush h

------------------------------------------------------------------------
-- state accessor functions

-- | Access a component of the state with a projection function
getsHS :: (HState -> a) -> IO a
getsHS f = f <$> readMVar hState

-- | Modify the state with a pure function
silentlyModifyHS :: (HState -> HState) -> IO ()
silentlyModifyHS  f = modifyMVar_ hState (pure . f)

modifyHS_ :: (HState -> HState) -> IO ()
modifyHS_ f = silentlyModifyHS f <* touchHS

-- | Modify the state returning a value
modifyHS :: (HState -> (HState, a)) -> IO a
modifyHS f = modifyMVar hState (pure . f) <* touchHS

-- | Trigger a refresh. This is the only way to update the screen.
touchHS :: IO ()
touchHS = withMVar hState \st -> void $ tryPutMVar (modified st) ()

