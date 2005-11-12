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
        shutdown
    ) where

import POpen
import Syntax
import State
import Lexers
import Curses
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
                                    hPutStrLn stderr (show e)
                                    exitWith (ExitFailure 1)) $ do

        -- fork process first. could fail. pass handles over to threads
        (r,w,pid) <- popen (MPG321 :: String) ["-R","-"]

        modifyState_ $ \s -> return s { mp3pid    = pid
                                      , music     = ms
                                      , current   = 0
                                      , pipe      = Just w } 
        -- initialise curses
        UI.start

        -- fork some threads
        t  <- forkIO inputLoop
        t' <- forkIO refreshLoop
        t''<- forkIO clockLoop
        modifyState_ $ \s -> return s { threads = [t,t',t''] } 

        -- start the first song
        send (Load (head ms))
        modifyState_ $ \s -> unsafeSetCurrent s (head ms)
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
                        catchJust ioErrors UI.refresh print

        -- | Once each second, wake up a and redraw the clock
        clockLoop :: IO ()
        clockLoop = repeatM_ $ do
                        threadDelay delay 
                        catchJust ioErrors UI.refreshClock print
                where
                  delay = 1000 * 500 -- 0.5 seconds

        -- | Handle keystrokes fed to us by curses
        inputLoop :: IO ()
        inputLoop = repeatM_ $ handle handler $ sequence_ . keymap =<< getKeys
            where
                getKeys = unsafeInterleaveIO $ do
                        c  <- UI.getKey
                        cs <- getKeys
                        return (c:cs) -- A lazy list of curses keys

                handler e | isJust (ioErrors e) = hPutStrLn stderr (show e)
                          | isExitCall e        = throwIO e     -- to main thread?
                          | otherwise           = hPutStrLn stderr (show e)
          
                isExitCall (ExitException _) = True
                isExitCall _ = False

-- | Main thread, main loop. Handle messages arriving over a pipe from
-- the decoder process. When shutdown kills the other end of the pipe,
-- hGetLine will fail, so we take that chance to exit.
run :: Handle -> IO ()
run r = do
    handle (\_ -> return ()) $ do
        s <- hGetLine r
        case parser s of
            Right m -> handleMsg m
            Left e  -> mapM_ (hPutStrLn stderr . ("ERROR " ++)) e
        run r

-- | Close most things
shutdown :: IO ()
shutdown = Control.Exception.handle (\_ -> return ()) $ do
        UI.end
        send Quit
        pid <- readSt mp3pid    -- wait for the process
        tds <- readSt threads   -- knock of our threads
        waitForProcess $ unsafeCoerce# pid      -- bit evil
        mapM_ killThread tds
        modifyState_ $ \st -> return st { mp3pid = 0, threads = [] }

------------------------------------------------------------------------
-- 
-- Write incoming messages from the encoder to the global state in the
-- right pigeon hole.
--
handleMsg :: Msg -> IO ()
handleMsg (T _) = return ()
handleMsg (F (File (Just f))) = modifyState_ $! \s -> unsafeSetCurrent s f
handleMsg (F (File Nothing))  = return () -- id3 tag.
handleMsg (I i)        = modifyState_ $! \s -> return s { info    = Just i }
handleMsg (S t)        = do
        modifyState_ $! \s -> return s { status  = t }
        when (t == Stopped) down -- move to next track

handleMsg (R f) = do
    modifyClock $! \_ -> return $ Just f
    b <- isEmptyMVar clockModified
    when (not b) $ do   -- force an immediate update if we've just skipped 
        catchJust ioErrors UI.refreshClock print
        takeMVar clockModified
        return ()

------------------------------------------------------------------------
--
-- Basic operations
--

seekLeft :: IO ()
seekLeft        = do
        f <- readClock id
        case f of Nothing -> return ()
                  Just (Frame { currentFrame = fr }) -> do
                        send $ Jump (max 0 (fr-100)) -- arbitrary
                        tryPutMVar clockModified () -- touch the modified MVar
                        return ()
                        
                        

seekRight :: IO ()
seekRight       = do
        f <- readClock id
        case f of Nothing -> return ()
                  Just g@(Frame { currentFrame = fr }) -> do
                        send $ Jump (fr + (min 100 (framesLeft g)))
                        tryPutMVar clockModified () -- touch the modified MVar
                        return ()

up :: IO ()
up   = do i <- readSt current
          m <- readSt music
          when (i > 0) $ do
                let f = m !! (i - 1)
                modifyState_ $ \s -> unsafeSetCurrent s f
                modifyState_ $ \s -> return s { status = Playing }
                send $ Load f

down :: IO ()
down = do i <- readSt current
          m <- readSt music
          when (i < length m - 1) $ do
                let f = m !! (i + 1)
                modifyState_ $ \s -> unsafeSetCurrent s f
                modifyState_ $ \s -> return s { status = Playing }
                send $ Load f

pause :: IO ()
pause = send Pause

quit :: IO ()
quit = shutdown >> exitWith ExitSuccess

------------------------------------------------------------------------
--
-- The keymap
--

keymap :: [Char] -> [IO ()]
keymap cs = let (actions,_,_) = execLexer mode (cs, ()) in actions

mode :: Lexer () (IO ())
mode = command

------------------------------------------------------------------------
-- 
-- LEFT    Seek left within song
-- RIGHT   Seek right within song
-- UP      Move up
-- DOWN    Move down
-- PGUP    Jump up
-- PGDN    Jump down
-- ENTER   Select song
-- SPACE   Pause/unpause
-- +       Increase volume for current song
-- -       Decrease volume for current song
-- n       Next song
-- /       Search within the playlist
-- A       Sort playlist according to Artist
-- S       Sort playlist according to Song title
-- T       Sort playlist according to Time
-- R       Sort playlist according to Rating
-- 1-9     Set rating of selected song
-- a       Add files to playlist
-- r       Enable/disable random play mode
-- l       Enable/disable loop play mode
-- e       Edit ID3 tags for selected song
-- s       Save playlist
-- c       Jump to currently playing song
-- Use arrows to move up/down and Q to close help screen  
 
command :: Lexer () (IO ())
command = cmd `action` \[c] -> Just $ case c of
                'q' -> quit
                k | k == keyUp    || k == 'k' -> up
                  | k == keyDown  || k == 'j' -> down
                k | k == keyLeft  || k == 'h' -> seekLeft
                  | k == keyRight || k == 'l' -> seekRight
                  | k == ' '      || k == 'p' -> pause
                _     -> return ()
        where cmd = alt $ "qkjhlp " ++ [keyUp, keyDown, keyLeft, keyRight]
