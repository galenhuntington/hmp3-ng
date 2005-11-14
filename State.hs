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

import Syntax
import Style 
import qualified Data.FastPackedString as P

import Data.IORef               ( newIORef, readIORef, writeIORef, IORef )
import System.IO.Unsafe         ( unsafePerformIO )

import Control.Concurrent       ( ThreadId )
import Control.Concurrent.MVar

import System.IO
import System.Posix.Types       ( ProcessID )

------------------------------------------------------------------------

-- | The editor state type
data State = State {
        music           :: ![(P.FastString,P.FastString)] -- TODO, sort on mp3 fields
       ,size            :: !Int             -- cache size of list
       ,current         :: !Int             -- currently playing mp3
       ,cursor          :: !Int             -- mp3 under the cursor

       ,mp3pid          :: ProcessID        -- pid of decoder
       ,pipe            :: Maybe Handle     -- r/w pipe to mp3
       ,threads         :: [ThreadId]       -- all our threads

       ,info            :: Maybe Info       -- mp3 info
       ,status          :: Status                  

       ,minibuffer      :: StringA          -- contents of minibuffer

       ,helpVisible     :: !Bool           -- is the help window shown
       ,mode            :: !Mode           -- random mode
    }

data Mode = Normal | Random | Loop
    deriving (Eq,Bounded,Enum)   -- for pred,succ


------------------------------------------------------------------------
--
-- | The initial state
--
emptySt :: State
emptySt = State {
        music        = []
       ,size         = 0
       ,mp3pid       = 0
       ,pipe         = Nothing
       ,threads      = []
       ,current      = 0
       ,cursor       = 0
       ,info         = Nothing
       ,status       = Stopped
       ,minibuffer   = Plain []
       ,helpVisible  = False
       ,mode         = Normal
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

-- | Modify the contents, using an IO action.
modifyState_ :: (State -> IO State) -> IO ()
modifyState_ f = modifyMVar_ state $ \r -> do
            v  <- readIORef r
            v' <- f v
            writeIORef r v'
            tryPutMVar modified ()
        --  hPutStrLn stderr "MODIFIED"
            return r

-- | Variation on modifyState_ that won't trigger a refresh
unsafeModifyState :: (State -> IO State) -> IO ()
unsafeModifyState f = modifyMVar_ state $ \r -> do
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
send :: Pretty a => Maybe Handle -> a -> IO ()
send mp m = case mp of
    Nothing -> hPutStrLn stderr "send: no pipe to send on"
    Just h  -> hPutStrLn h (draw m) >> hFlush h
