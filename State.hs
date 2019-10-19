-- 
-- Copyright (c) 2004-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019 Galen Huntington
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
-- | The top level application state, and operations on that value.
--
module State where

import FastIO (FiltHandle(..))
import Syntax                   (Status(Stopped), Mode(..), Frame, Info,Id3)
import Tree                     (FileArray, DirArray)
import Style                    (StringA(Fast), defaultSty, UIStyle)
import qualified Data.ByteString as P (empty,ByteString)
import qualified Config (defaultStyle)

import Text.Regex.PCRE.Light    (Regex)
import Data.Array               (listArray)
import System.IO.Unsafe         (unsafePerformIO)
import System.Posix.Types       (ProcessID)
import System.Clock             (TimeSpec(..))
import System.IO                (Handle)
import System.Random.Mersenne

import Control.Concurrent       (ThreadId)
import Control.Concurrent.MVar
import Data.IORef

-- import Control.Monad.State

------------------------------------------------------------------------
-- A state monad over IO would be another option.

-- type ST = StateT HState IO

------------------------------------------------------------------------

-- | The editor state type
data HState = HState {
        music           :: !FileArray
       ,folders         :: !DirArray
       ,size            :: !Int                  -- cache size of list
       ,current         :: !Int                  -- currently playing mp3
       ,cursor          :: !Int                  -- mp3 under the cursor
       ,clock           :: !(Maybe Frame)        -- current clock value
       ,clockUpdate     :: !Bool
       ,mp3pid          :: !(Maybe ProcessID)    -- pid of decoder
       ,writeh          :: !(MVar Handle)        --  handle to mp3 (should be MVars?)
       ,errh            :: !(MVar FiltHandle)    --  error handle to mp3
       ,readf           :: !(MVar FiltHandle)    -- r/w pipe to mp3
       ,threads         :: ![ThreadId]           -- all our threads
       ,id3             :: !(Maybe Id3)          -- maybe mp3 id3 info
       ,info            :: !(Maybe Info)         -- mp3 info
       ,status          :: !Status
       ,minibuffer      :: !StringA              -- contents of minibuffer
       ,helpVisible     :: !Bool                 -- is the help window shown
       ,miniFocused     :: !Bool                 -- is the mini buffer focused?
       ,mode            :: !Mode                 -- random mode
       ,uptime          :: !P.ByteString
       ,boottime        :: !TimeSpec
       ,regex           :: !(Maybe (Regex,Bool)) -- most recent search pattern and direction
       ,xterm           :: !Bool
       ,doNotResuscitate:: !Bool                -- should we just let mpg321 die?
       ,config          :: !UIStyle             -- config values

       ,modified        :: !(MVar ())           -- Set when redrawable components of 
                                                -- the state are modified. The ui
                                                -- refresh thread waits on this.
       ,randomGen       :: MTGen
    }

------------------------------------------------------------------------
--
-- | The initial state
--
emptySt :: HState
emptySt = HState {
        music        = listArray (0,0) []
       ,folders      = listArray (0,0) []

       ,size         = 0
       ,current      = 0
       ,cursor       = 0

       ,threads      = []
       ,modified     = unsafePerformIO newEmptyMVar
       ,writeh       = unsafePerformIO newEmptyMVar
       ,errh         = unsafePerformIO newEmptyMVar
       ,readf        = unsafePerformIO newEmptyMVar

       ,mp3pid       = Nothing
       ,clock        = Nothing
       ,info         = Nothing
       ,id3          = Nothing
       ,regex        = Nothing

       ,clockUpdate      = False
       ,helpVisible      = False
       ,miniFocused      = False
       ,xterm            = False
       ,doNotResuscitate = False    -- mgp321 should be be restarted

       ,config       = Config.defaultStyle
       ,boottime     = TimeSpec 0 0
       ,status       = Stopped
       ,mode         = Normal
       ,minibuffer   = Fast P.empty defaultSty
       ,uptime       = P.empty
       ,randomGen    = unsafePerformIO (newMTGen Nothing)
    }

--
-- | A global variable holding the state. Todo StateT
--
state :: MVar HState
state = unsafePerformIO $ newMVar emptySt
{-# NOINLINE state #-}

------------------------------------------------------------------------
-- state accessor functions

-- | Access a component of the state with a projection function
getsST :: (HState -> a) -> IO a
getsST f = withST (return . f)

-- | Perform a (read-only) IO action on the state
withST :: (HState -> IO a) -> IO a
withST f = readMVar state >>= f

-- | Modify the state with a pure function
silentlyModifyST :: (HState -> HState) -> IO ()
silentlyModifyST  f = modifyMVar_ state (return . f)

------------------------------------------------------------------------

modifyST :: (HState -> HState) -> IO ()
modifyST f = silentlyModifyST f >> touchST

-- | Modify the state with an IO action, triggering a refresh
modifySTM :: (HState -> IO HState) -> IO ()
modifySTM f = modifyMVar_ state f >> touchST

-- | Modify the state with an IO action, returning a value
modifySTM_ :: (HState -> IO (HState,a)) -> IO a
modifySTM_ f = modifyMVar state f >>= \a -> touchST >> return a

-- | Trigger a refresh. This is the only way to update the screen
touchST :: IO ()
touchST = withMVar state $ \st -> tryPutMVar (modified st) () >> return ()

forceNextPacket :: IO ()
forceNextPacket = do
  fh <- readMVar =<< getsST readf
  writeIORef (frameCount fh) 0

