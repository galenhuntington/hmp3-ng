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
        seekLeft, seekRight, up, down, pause, quit, clrmsg
    ) where

import POpen
import Syntax
import State
import Style
import Config
import Utils
import qualified UI

import Data.Maybe

import Control.Monad

import System.IO
import System.Exit

import Control.Concurrent

import System.Process           ( waitForProcess )
import Control.Exception

import GHC.Base
import GHC.Exception
import GHC.IOBase               ( unsafeInterleaveIO )

#include "config.h"

------------------------------------------------------------------------

start :: [FilePath] -> IO ()
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

        modifyState_ $ \s -> return s { mp3pid    = pid
                                      , music     = [ (m, basename m) | m <- ms ]
                                      , current   = 0
                                      , pipe      = Just w } 

        -- fork some threads
        t  <- forkIO inputLoop
        t' <- forkIO refreshLoop
        t''<- forkIO clockLoop
        modifyState_ $ \s -> return s { threads = [t,t',t''] } 

        -- getting wmove errors for some reason

        -- start the first song
            
        send (Just w) (Load (head ms))
        modifyState_ $ \s -> return s { status = Playing }

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
                  delay = 1000 * 1000 -- 0.5 seconds

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
            tds = threads st  -- knock of our threads
        send (pipe st) Quit
        waitForProcess $ unsafeCoerce# pid      -- bit evil
        mapM_ killThread tds
        return st { mp3pid = 0, threads = [] }

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
        when (t == Stopped) down -- (or random, or loop) move to next track

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

-- be careful for races:
up :: IO ()
up = modifyState_ $ \st -> do
    let i = current st
        m = music st
    if i > 0
        then do let (f,_) = m !! (i - 1)
                    st' = st { current = (i - 1), status  = Playing }
                send (pipe st) (Load f)
                return st'
        else return st

down :: IO ()
down = modifyState_ $ \st -> do
    let i = current st
        m = music st
    if i < length m - 1
        then do let (f,_) = m !! (i + 1)
                    st' = st { current = (i + 1), status  = Playing }
                send (pipe st) (Load f)
                return st'
        else return st

pause :: IO ()
pause = withState $ \st -> send (pipe st) Pause

quit :: IO ()
quit = shutdown {-  >> throwTo main thread exitWith ExitSuccess -}

------------------------------------------------------------------------
-- Editing the minibuffer

putmsg :: StringA -> IO ()
putmsg s = do
    hPutStrLn stderr $ "PUTMSG" ++ show s
    unsafeModifyState $ \st -> return st { minibuffer = s }

-- Modify without triggering a refresh
clrmsg :: IO ()
clrmsg = unsafeModifyState $ \s -> return s { minibuffer = empty }
    where empty = Plain []

-- showA :: Show a => a -> IO ()
-- showA = putmsg . Plain . show

warnA :: Show a => a -> IO ()
warnA = putmsg . Fancy . map (\c -> A c (warnings (style config))) . show

-- unsafeWarnA :: State -> String -> State
-- unsafeWarnA st s = st { minibuffer = Fancy (map (\c -> A c (warnings (style config))) s) }

