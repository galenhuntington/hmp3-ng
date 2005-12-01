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

import Syntax                   (Pretty(ppr),Status(Stopped), Frame, Info,Id3)
import Tree                     (FileArray, DirArray)
import Style                    (StringA(Fast), defaultSty)
import Regex                    (Regex)
import qualified Data.FastPackedString as P (empty,FastString,hPut,pack)

import Data.Array               (listArray)
import Data.IORef               (newIORef,readIORef,writeIORef,IORef)
import System.IO                (Handle, hFlush)
import System.IO.Unsafe         (unsafePerformIO)
import System.Posix.Types       (ProcessID)
import System.Time              (ClockTime(..))
import Foreign.C.Types          (CFile)
import Foreign.Ptr              (Ptr)

import Control.Concurrent       (ThreadId)
import Control.Concurrent.MVar

------------------------------------------------------------------------

-- | The editor state type
data State = State {
        music           :: !FileArray
       ,folders         :: !DirArray
       ,size            :: !Int             -- cache size of list
       ,current         :: !Int             -- currently playing mp3
       ,cursor          :: !Int             -- mp3 under the cursor
       ,mp3pid          :: !(Maybe ProcessID)       -- pid of decoder
       ,writeh          :: !(MVar Handle)     --  handle to mp3 (should be MVars?)
       ,errh            :: !(MVar Handle)     --  error handle to mp3
       ,readf           :: !(MVar (Ptr CFile))-- r/w pipe to mp3
       ,threads         :: ![ThreadId]       -- all our threads
       ,id3             :: !(Maybe Id3)        -- maybe mp3 id3 info
       ,info            :: !(Maybe Info)       -- mp3 info
       ,status          :: !Status                  
       ,minibuffer      :: !StringA          -- contents of minibuffer
       ,helpVisible     :: !Bool           -- is the help window shown
       ,miniFocused     :: !Bool           -- is the mini buffer focused?
       ,mode            :: !Mode           -- random mode
       ,uptime          :: !P.FastString
       ,boottime        :: !ClockTime
       ,regex           :: !(Maybe (Regex,Bool))   -- most recent search pattern and direction
       ,xterm           :: !Bool
       ,doNotResuscitate:: !Bool            -- should we just let mpg321 die?
    }

data Mode = Normal | Loop | Random deriving (Eq,Bounded,Enum) -- for pred,succ

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
-- | A global variable holding the current frame (lots of updates), but
-- we only care about the most recent one.
--
clock :: MVar (IORef (Maybe Frame))
clock = unsafePerformIO $ do
            ref  <- newIORef Nothing
            newMVar ref
{-# NOINLINE clock #-}

readClock :: ((Maybe Frame) -> b) -> IO b
readClock f = withMVar clock $ \ref -> return . f =<< readIORef ref

modifyClock :: ((Maybe Frame) -> IO (Maybe Frame)) -> IO ()
modifyClock f = modifyMVar_ clock $ \r -> do
    v  <- readIORef r
    v' <- f v
    writeIORef r v'
    return r

------------------------------------------------------------------------
--
-- Set when redrawable components of the state are modified. The ui
-- refresh thread waits on this.
modified :: MVar ()
modified = unsafePerformIO $ newMVar ()
{-# NOINLINE modified #-}

-- | When set, we are supposed to update the clock immediately 
clockModified :: MVar ()
clockModified = unsafePerformIO $ newMVar ()
{-# NOINLINE clockModified #-}

------------------------------------------------------------------------
-- state accessor functions

-- | Read the state, with an IO action
withState :: (State -> IO ()) -> IO ()
withState f = withMVar state $ \ref -> f =<< readIORef ref

-- | Read state
readState :: (State -> a) -> IO a
readState f = withMVar state $ \ref -> return . f =<< readIORef ref

-- | Modify the contents, using an IO action.
modifyState_ :: (State -> IO State) -> IO ()
modifyState_ f = modifyMVar_ state $ \r -> do
    v  <- readIORef r
    v' <- f v
    writeIORef r v'
    tryPutMVar modified ()
    return r

-- | Trigger a refresh
touchState :: IO ()
touchState = modifyState_ $ return . id

-- | Variation on modifyState_ that won't trigger a refresh
silentlyModifyState :: (State -> IO State) -> IO ()
silentlyModifyState f = modifyMVar_ state $ \r -> do
    v  <- readIORef r
    v' <- f v
    writeIORef r v'
    return r

-- | Variation on modifyState_ that lets you return a value
modifyState :: (State -> IO (State,b)) -> IO b
modifyState f = modifyMVar state $ \r -> do
    v  <- readIORef r
    (v',b) <- f v
    writeIORef r v'
    tryPutMVar modified ()
    return (r,b)

------------------------------------------------------------------------

-- | Send a msg over the channel to the decoder
send :: Pretty a => Handle -> a -> IO ()
send h m = P.hPut h (ppr m) >> P.hPut h nl >> hFlush h
    where
      nl = P.pack "\n"

