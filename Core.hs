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
-- | Main module. 
--
module Core (
        start,
        shutdown,
        seekLeft, seekRight, up, down, pause, nextMode, playNext,
        quit, clrmsg, toggleHelp, play, jumpToPlaying
    ) where

import POpen
import Syntax
import State
import Style
import Config
import Utils
import qualified UI

import qualified Data.FastPackedString as P

import Data.Maybe

import Control.Monad

import System.IO
import System.Exit
import System.Random            (getStdRandom, randomR)

import Control.Concurrent

import System.Process           ( waitForProcess )
import Control.Exception

import GHC.Base
import GHC.Exception
import GHC.IOBase               ( unsafeInterleaveIO )

#include "config.h"

------------------------------------------------------------------------

start :: [P.FastString] -> IO ()
start ms = 
    Control.Exception.handle
        (\e -> do if isOK e then return ()
                            else do shutdown
                                    hPutStrLn stderr ("start: " ++ show e)
                                    exitWith (ExitFailure 1)) $ do

        -- initialise curses
        UI.start

        -- fork process first. could fail. pass handles over to threads
        (r,w,pid) <- popen (MPG321 :: String) ["-R","-"]

        modifyState_ $ \s -> return s 
            { mp3pid    = pid
            , music     = [ (m, basenameP m) | m <- ms ] -- look ma! no boxes!
            , size      = length ms
            , current   = 0
            , cursor    = 0
            , pipe      = Just w } 

        -- fork some threads
        t  <- forkIO inputLoop
        t' <- forkIO refreshLoop
        t''<- forkIO clockLoop
        modifyState_ $ \s -> return s { threads = [t,t',t''] } 

        -- start the first song
        play

        -- start the main loop
        run r
        shutdown

    where
        isOK (ExitException ExitSuccess) = True
        isOK _ = False
 
        -- | When the editor state has been modified, refresh, then wait
        -- for it to be modified again.
        refreshLoop :: IO ()
        refreshLoop = repeatM_ $ do
                        takeMVar modified
                        catchJust ioErrors UI.refresh warnA

        -- | Once each second, wake up a and redraw the clock
        clockLoop :: IO ()
        clockLoop = repeatM_ $ do
                        threadDelay delay 
                    --  hPutStrLn stderr "CLOCK"
                        catchJust ioErrors UI.refreshClock warnA
                where
                  delay = 1000 * 1000 -- 1 second

        -- | Handle keystrokes fed to us by curses
        inputLoop :: IO ()
        inputLoop = repeatM_ $ handle handler $ 
                        sequence_ . (keymap config) =<< getKeys
            where
                getKeys = unsafeInterleaveIO $ do
                        c  <- UI.getKey
                   --   hPutStrLn stderr "INPUT"
                        cs <- getKeys
                        return (c:cs) -- A lazy list of curses keys

                handler e | isJust (ioErrors e) = warnA e
                          | isExitCall e        = throwIO e     -- to main thread?
                          | otherwise           = warnA e
          
                isExitCall (ExitException _) = True
                isExitCall _ = False

-- | Main thread, main loop. Handle messages arriving over a pipe from
-- the decoder process. When shutdown kills the other end of the pipe,
-- hGetLine will fail, so we take that chance to exit.
run :: Handle -> IO ()
run r = do
    handle (\_ -> return ()) $ do
        s <- hGetLine r
    --  hPutStrLn stderr "MPG321"
        case parser s of
            Right m -> handleMsg m
            Left e  -> warnA e  -- error from pipe
        run r

-- | Close most things
shutdown :: IO ()
shutdown = Control.Exception.handle (\_ -> return ()) $ 
    modifyState_ $ \st -> do    -- atomic
        UI.end
        let pid = mp3pid st   -- wait for the process
            tds = threads st  -- knock off our threads
        send (pipe st) Quit
        waitForProcess $ unsafeCoerce# pid      -- bit evil
        -- send sigKILL?
        mapM_ killThread tds
        return st { mp3pid = (-1){-?-}, threads = [] }

------------------------------------------------------------------------
-- 
-- Write incoming messages from the encoder to the global state in the
-- right pigeon hole.
--
handleMsg :: Msg -> IO ()
handleMsg (T _)               = return ()
handleMsg (F (File (Just _))) = return () -- ignore, and hope we did the right thing
handleMsg (F (File Nothing))  = return () -- id3 tag.
handleMsg (I i)               = modifyState_ $! \s -> return s { info = Just i }

handleMsg (S t) = do
        modifyState_ $! \s -> return s { status  = t }
        when (t == Stopped) $ do   -- transition to next song
            r <- modifyState $ \st -> return (st, mode st) -- race
            if r == Random then playRandom else playNext

handleMsg (R f) = do
    modifyClock $! \_ -> return $ Just f
    b <- isEmptyMVar clockModified
    when (not b) $ do   -- force an immediate update if we've just skipped 
        takeMVar clockModified
        catchJust ioErrors UI.refreshClock warnA
        return ()

------------------------------------------------------------------------
--
-- Basic operations
--

-- | Seek backward in song
seekLeft :: IO ()
seekLeft        = do
    f <- readClock id
    case f of 
        Nothing -> return ()
        Just (Frame { currentFrame = fr }) -> do
            withState $ \st -> 
                send (pipe st) $ Jump (max 0 (fr-100))
            tryPutMVar clockModified () -- touch the modified MVar
            return ()

-- | Seek forward in song
seekRight :: IO ()
seekRight       = do
    f <- readClock id
    case f of 
        Nothing -> return ()
        Just g@(Frame { currentFrame = fr }) -> do
                withState $ \st -> 
                    send (pipe st) $ Jump (fr + (min 100 (framesLeft g)))
                tryPutMVar clockModified () -- touch the modified MVar
                return ()

-- | Move cursor up
up :: IO ()
up = modifyState_ $ \st -> do
    let i = cursor st
    return $ if i > 0 then st { cursor = i - 1 } else st

-- | Move cursor down list
down :: IO ()
down = modifyState_ $ \st -> do
        let i = cursor st
            m = music st
            l = size st - 1
        return $ if i == l
            then st 
            else st { cursor = i + 1 }

-- | Toggle pause on the current song
pause :: IO ()
pause = withState $ \st -> send (pipe st) Pause

-- | Load and play the song under the cursor
play :: IO ()
play = modifyState_ $ \st -> do
    let i     = cursor st
        m     = music st
        (f,_) = m !! i
        st'   = st { current = i, status = Playing }
    send (pipe st) (Load f)
    return st'

-- | Play the song following the current song, if we're not at the end
-- If we're at the end, and loop mode is on, then loop to the start
playNext :: IO ()
playNext = modifyState_ $ \st -> do
    let i   = current st
        m   = music st
    case () of {_ 
        | i < size st - 1          -- successor
        -> let (f,_) = m !! (i + 1)
               st'   = st { current = i + 1, status = Playing } 
           in send (pipe st) (Load f) >> return st'

        | mode st == Loop           -- else loop
        -> let (f,_) = m !! 0
               st'   = st { current = i + 1, status = Playing } 
           in send (pipe st) (Load f) >> return st'

        | otherwise -> return st    -- else stop
    }

-- | Play a random song
playRandom :: IO ()
playRandom = modifyState_ $ \st -> do
    let m   = music st
    i <- getStdRandom (randomR (0, size st - 1)) -- memoise length m?
    let (f,_) = m !! i
        st'   = st { current = i, status = Playing } 
    send (pipe st) (Load f)
    return st'

-- | Shutdown and exit
quit :: IO ()
quit = shutdown

-- | Move cursor to currently playing song
jumpToPlaying :: IO ()
jumpToPlaying = modifyState_ $ \st -> return st { cursor = (current st) }

-- | Show/hide the help window
toggleHelp :: IO ()
toggleHelp = modifyState_ $ \st -> return st { helpVisible = not (helpVisible st) }

-- | Toggle the mode flag
nextMode :: IO ()
nextMode = modifyState_ $ \st -> return st { mode = next (mode st) }
    where 
        next v = if v == maxBound then minBound else succ v

------------------------------------------------------------------------
-- Editing the minibuffer

putmsg :: StringA -> IO ()
putmsg s = unsafeModifyState $ \st -> return st { minibuffer = s }

-- Modify without triggering a refresh
clrmsg :: IO ()
clrmsg = unsafeModifyState $ \s -> return s { minibuffer = empty }
    where empty = Plain []

-- showA :: Show a => a -> IO ()
-- showA = putmsg . Plain . show

warnA :: Show a => a -> IO ()
warnA = putmsg . Fancy . map (\c -> A c (warnings (style config))) . concat . take 1 . lines . show

-- unsafeWarnA :: State -> String -> State
-- unsafeWarnA st s = st { minibuffer = Fancy (map (\c -> A c (warnings (style config))) s) }

