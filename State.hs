-- 
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

import Syntax                   (Status(Stopped), Mode(..), Frame, Info,Id3)
import Tree                     (FileArray, DirArray)
import Style                    (StringA(Fast), defaultSty, UIStyle)
import Regex                    (Regex)
import qualified Data.FastPackedString as P (empty,FastString)
import qualified Config (defaultStyle)

import Data.Array               (listArray)
import Data.IORef               (newIORef,readIORef,writeIORef,IORef)
import System.IO.Unsafe         (unsafePerformIO)
import System.Posix.Types       (ProcessID)
import System.Time              (ClockTime(..))
import System.IO                (Handle)
import Foreign.C.Types          (CFile)
import Foreign.Ptr              (Ptr)

import Control.Concurrent       (ThreadId)
import Control.Concurrent.MVar

------------------------------------------------------------------------
-- The ST monad over IO

-- type ST = StateT State IO

------------------------------------------------------------------------

-- | The editor state type
data State = State {
        music           :: !FileArray
       ,folders         :: !DirArray
       ,size            :: !Int                  -- cache size of list
       ,current         :: !Int                  -- currently playing mp3
       ,cursor          :: !Int                  -- mp3 under the cursor
       ,clock           :: !(Maybe Frame)        -- current clock value
       ,clockUpdate     :: !Bool
       ,mp3pid          :: !(Maybe ProcessID)    -- pid of decoder
       ,writeh          :: !(MVar Handle)        --  handle to mp3 (should be MVars?)
       ,errh            :: !(MVar Handle)        --  error handle to mp3
       ,readf           :: !(MVar (Ptr CFile))   -- r/w pipe to mp3
       ,threads         :: ![ThreadId]           -- all our threads
       ,id3             :: !(Maybe Id3)          -- maybe mp3 id3 info
       ,info            :: !(Maybe Info)         -- mp3 info
       ,status          :: !Status                  
       ,minibuffer      :: !StringA              -- contents of minibuffer
       ,helpVisible     :: !Bool                 -- is the help window shown
       ,miniFocused     :: !Bool                 -- is the mini buffer focused?
       ,mode            :: !Mode                 -- random mode
       ,uptime          :: !P.FastString
       ,boottime        :: !ClockTime
       ,regex           :: !(Maybe (Regex,Bool)) -- most recent search pattern and direction
       ,xterm           :: !Bool
       ,doNotResuscitate:: !Bool                -- should we just let mpg321 die?
       ,config          :: !UIStyle             -- config values
    }

------------------------------------------------------------------------
--
-- | The initial state
--
emptySt :: State
emptySt = State {
        music        = listArray (0,0) []
       ,folders      = listArray (0,0) []
       ,size         = 0
       ,mp3pid       = Nothing
       ,clock        = Nothing
       ,clockUpdate  = False
       ,writeh       = unsafePerformIO newEmptyMVar
       ,errh         = unsafePerformIO newEmptyMVar
       ,readf        = unsafePerformIO newEmptyMVar
       ,threads      = []
       ,current      = 0
       ,cursor       = 0
       ,info         = Nothing
       ,id3          = Nothing
       ,status       = Stopped
       ,minibuffer   = Fast P.empty defaultSty
       ,helpVisible  = False
       ,miniFocused  = False
       ,mode         = Normal
       ,uptime       = P.empty
       ,boottime     = TOD 0 0
       ,regex        = Nothing
       ,xterm        = False
       ,doNotResuscitate = False    -- mgp321 should be be restarted
       ,config       = Config.defaultStyle
    }

--
-- | A global variable holding the state
--
state :: MVar (IORef State)
state = unsafePerformIO $ do
            ref  <- newIORef emptySt
            newMVar ref
{-# NOINLINE state #-}

------------------------------------------------------------------------
--
-- Set when redrawable components of the state are modified. The ui
-- refresh thread waits on this.
modified :: MVar ()
modified = unsafePerformIO $ newMVar ()
{-# NOINLINE modified #-}

------------------------------------------------------------------------
-- state accessor functions

-- | Read the state, with an IO action
withState :: (State -> IO a) -> IO a
withState f = withMVar state $ \ref -> f =<< readIORef ref

-- | Read state
readState :: (State -> a) -> IO a
readState f = withState (return . f)

-- | Modify the contents, using an IO action.
modifyState_ :: (State -> IO State) -> IO ()
modifyState_ f = modifyState $ \st -> do st' <- f st; return (st',())

-- | Variation on modifyState_ that lets you return a value
modifyState :: (State -> IO (State,b)) -> IO b
modifyState f = doModifyState f >>= \b -> touchState >> return b

-- | Variation on modifyState_ that won't trigger a refresh
silentlyModifyState :: (State -> State) -> IO ()
silentlyModifyState f = doModifyState $ \st -> return (f st, ())

-- | IO version
silentlyModifyStateM :: (State -> IO State) -> IO ()
silentlyModifyStateM f = doModifyState $ \st -> do st' <- f st; return (st', ())

-- | Trigger a refresh
touchState :: IO ()
touchState = tryPutMVar modified () >> return ()

-- worker
doModifyState :: (State -> IO (State,b)) -> IO b
doModifyState f = modifyMVar state $ \r -> do
    v      <- readIORef r
    (v',b) <- f v
    writeIORef r v'
    return (r,b)
