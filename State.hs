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

import Data.List
import Data.IORef               ( newIORef, readIORef, writeIORef, IORef )
import System.IO.Unsafe         ( unsafePerformIO )

import Control.Concurrent       ( ThreadId )
import Control.Concurrent.MVar

import System.IO
import System.Posix.Types   ( ProcessID )

------------------------------------------------------------------------

-- | The editor state type
data State = State {
        music           :: ![FilePath]                  -- todo, sort on mp3 fields
       ,current         :: Int                          -- current playing mp3

       ,mp3pid          :: ProcessID                    -- pid of decoder
       ,pipe            :: Maybe Handle                 -- r/w pipe to mp3
       ,threads         :: [ThreadId]                   -- all our threads

       ,info            :: Maybe Info                   -- mp3 info
       ,status          :: Status                  
    }


------------------------------------------------------------------------
--
-- | The initial state
--
emptySt :: State
emptySt = State {
        music        = []

       ,mp3pid       = 0
       ,pipe         = Nothing
       ,threads      = []

       ,current      = (-1)
       ,info         = Nothing
       ,status       = Stopped
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

------------------------------------------------------------------------
-- state accessor functions

-- | Read the state, with a pure action
readSt :: (State -> b) -> IO b
readSt f = withMVar state $ \ref -> return . f =<< readIORef ref

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
-- manipulating specific parts of the state

-- | Give the current mp3 from the decoder, update our state
unsafeSetCurrent  :: State -> FilePath -> IO State
unsafeSetCurrent s f
    | Just i <- findIndex (==f) (music s)
    = return s { current = i }

    | Just i <- findIndex (\k -> f `isPrefixOf` k) (music s)
    = return s { current = i }

    | Just i <- findIndex (\k -> (f ++ ".mp3") `isSuffixOf` k) (music s)
    = return s { current = i }

    | otherwise = do hPutStrLn stderr $ "unsafeSetCurrent: unknown track: " ++ show f
                     return s

------------------------------------------------------------------------

-- | Send a msg over the channel to the decoder
send :: Pretty a => a -> IO ()
send m = do w <- readSt pipe
            case w of
                Nothing -> hPutStrLn stderr "send. no pipe to send on"
                Just x -> do
                    let s = draw m
                    hPutStrLn x s
                    hFlush x

