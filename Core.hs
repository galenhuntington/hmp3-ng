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
        quit, putmsg, clrmsg, toggleHelp, play, jumpToPlaying, jump, {-, add-}
        writeSt, readSt, jumpToMatch, toggleFocus, jumpToNextDir, jumpToPrevDir
    ) where

import Prelude hiding (catch)

import POpen
import Syntax
import Lexer
import State
import Style
import Config
import Utils
import FastIO       ( fdToCFile, joinPathP )
import Tree hiding (File)
import Regex
import qualified UI

import qualified Data.FastPackedString as P

import Data.Maybe
import Data.Array

import Control.Monad

import System.IO
import System.Exit
import System.Time
import System.Random            ( getStdRandom, randomR )
import System.Process           ( waitForProcess )
import System.Environment       ( getEnv )

import Control.Concurrent
import Control.Exception

import System.Directory
import System.Posix.User        ( getUserEntryForID, getRealUserID, homeDirectory )
import System.Posix.Process     ( exitImmediately )

import GHC.Base
import GHC.Handle
import GHC.Exception hiding     ( catch )
import GHC.IOBase               ( unsafeInterleaveIO )

#include "config.h"

------------------------------------------------------------------------

start :: Either (FileArray,DirArray,Int) [P.FastString] -> IO ()
start ms = do

    t0 <- forkIO mpgLoop    -- start this off early, to give mpg321 a time to settle
    t5 <- forkIO errorLoop

    UI.start -- initialise curses

    (ds,fs,i) <- case ms of -- construct the state
                Left (fs',ds',x) -> return (ds',fs',x)
                Right roots      -> buildTree roots >>= \(a,b) -> return (a,b,0)

    now   <- getClockTime
    modifyState_ $ \s -> return s 
        { music     = fs
        , folders   = ds
        , size      = 1 + (snd . bounds $ fs)
        , cursor    = i
        , current   = i
        , uptime    = drawUptime now now
        , boottime  = now }

    -- fork some threads
    t1 <- forkIO inputLoop
    t2 <- forkIO refreshLoop
    t3 <- forkIO clockLoop
    t4 <- forkIO uptimeLoop
    modifyState_ $ \s -> return s { threads = [t0,t1,t2,t3,t4,t5] } 

    when (0 <= (snd . bounds $ fs)) play -- start the first song

    run         -- won't restart if this fails!
    shutdown    -- can this even happen?

------------------------------------------------------------------------

-- | Process loop, launch mpg321, set the handles in the state
-- and then wait for the process to die. If it does, restart it.
mpgLoop :: IO ()
mpgLoop = handle (\e -> (warnA.show) e >> mpgLoop) $ do
    (r,w,e,pid) <- popen (MPG321 :: String) ["-R","-"]
    hw          <- fdToHandle (unsafeCoerce# w)  -- so we can use Haskell IO
    ew          <- fdToHandle (unsafeCoerce# e)  -- so we can use Haskell IO
    filep       <- fdToCFile r                   -- so we can use C IO
    modifyState_ $ \st -> return st { mp3pid    = pid
                                    , writeh    = Just hw
                                    , errh      = Just ew
                                    , readf     = Just filep 
                                    , status    = Stopped
                                    , info      = Nothing
                                    , id3       = Nothing }
    Control.Exception.catch (waitForProcess $ unsafeCoerce# pid)
                            (\_ -> return ExitSuccess)

    warnA (MPG321 ++ " restarting")

    stop <- readState doNotResuscitate -- more races
    when (not stop) mpgLoop
    -- and if it returns, loop.

------------------------------------------------------------------------

-- | When the editor state has been modified, refresh, then wait
-- for it to be modified again.
refreshLoop :: IO ()
refreshLoop = repeatM_ $ handle (warnA.show) $ do
        takeMVar modified
        catchJust ioErrors UI.refresh (warnA.show)

------------------------------------------------------------------------

-- | Once a minute read the clock time
uptimeLoop :: IO ()
uptimeLoop = repeatM_ $ handle (warnA.show) $ do
        threadDelay delay 
        now <- getClockTime 
        modifyState_ $ \st -> do
            let t = drawUptime (boottime st) now
         -- hPutStrLn stderr ("TIME " ++ show t)
            return st { uptime = t }
    where
        delay = 60 * 1000 * 1000 -- 1 minute

------------------------------------------------------------------------

-- | Once each second, wake up a and redraw the clock
clockLoop :: IO ()
clockLoop = repeatM_ $ handle (warnA.show) $ do
        threadDelay delay 
    --  hPutStrLn stderr "CLOCK"
        catchJust ioErrors UI.refreshClock (warnA.show)
    where
        delay = 500 * 1000 -- 0.5 second

------------------------------------------------------------------------

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

        handler e | isJust (ioErrors e) = (warnA.show) e
                  | isExitCall e        = (warnA.show) e >> throwIO e
                  | otherwise           = (warnA.show) e
  
        isExitCall (ExitException _) = True
        isExitCall _ = False

------------------------------------------------------------------------

-- | Handle errors produced by mpg321
errorLoop :: IO ()
errorLoop = repeatM_ $ handle (warnA.show) $ do
    mh   <- readState errh -- race
    case mh of
        Nothing -> warnA "No error handle to mpg321"
        Just h  -> hGetLine h >>= warnA

------------------------------------------------------------------------

-- | Main thread, main loop. Handle messages arriving over a pipe from
-- the decoder process. When shutdown kills the other end of the pipe,
-- hGetLine will fail, so we take that chance to exit.
--
-- NB, if we ever want to do profiling, insert a throwIO as the handler,
-- and replace exitImmediately with return ()
--
run :: IO ()
run = repeatM_ $ handle (warnA.show) $ do        -- don't stop
    mp   <- readState readf -- race
    case mp of
        Nothing -> warnA "No handle to mpg321"
        Just p  -> do 
            res <- parser p
            case res of
                Right m -> handleMsg m
                Left e  -> (warnA.show) e  -- error from pipe

------------------------------------------------------------------------

-- | Close most things. Important to do all the jobs:
shutdown :: IO ()
shutdown = handle (\e -> hPutStrLn stderr (show e) >> return ()) $ do
    -- so this should stop the mpg restart thread looping
    modifyState_ $ \st -> return st { doNotResuscitate = True }

    writeSt

    modifyState_ $ \st -> do
        UI.end (xterm st)

        let pid = mp3pid st
            tds = threads st

        -- now shtudown mpg321
        handle (\_ -> return ()) $ do
            send (writeh st) Quit                        -- ask politely
            waitForProcess $ unsafeCoerce# pid
            return ()

--      Should check if it's still running
--      handle (\_ -> return ()) $
--          signalProcess sigTERM (unsafeCoerce# pid)   -- just kill it

        -- now our own threads
        flip mapM_ tds $ \t -> 
            catch (killThread t) (\_ -> return ())      -- and kill threads

        return st { mp3pid = (-1){-?-}, threads = [] } -- just being safe

    -- and get the hell out of here
    exitImmediately ExitSuccess

------------------------------------------------------------------------
-- 
-- Write incoming messages from the encoder to the global state in the
-- right pigeon hole.
--
handleMsg :: Msg -> IO ()
handleMsg (T _)                = return ()

handleMsg (F (File (Left  _))) =
    modifyState_ $! \s -> return s { id3 = Nothing }
handleMsg (F (File (Right i))) =
    modifyState_ $! \s -> return s { id3 = Just i }

handleMsg (I i) = modifyState_ $! \s -> return s { info = Just i }

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
        catchJust ioErrors UI.refreshClock (warnA.show)
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
                send (writeh st) $ Jump (max 0 (fr-400))
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
                    send (writeh st) $ Jump (fr + (min 400 (framesLeft g)))
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
        l = size st - 1
    return $ if i == l then st else st { cursor = i + 1 }

-- | Move cursor to specified index
jump :: Int -> IO ()
jump i = modifyState_ $ \st -> do
    let l = size st - 1
        n = if i > l then l else if i < 0 then 0 else i
    return st { cursor = n }

-- | Toggle pause on the current song
pause :: IO ()
pause = withState $ \st -> send (writeh st) Pause

-- | Load and play the song under the cursor
play :: IO ()
play = modifyState_ $ \st -> do
    let i    = cursor st
        m    = music st
        f    = let fe = m ! i in (dname $ folders st ! fdir fe) `joinPathP` (fbase fe)
        st'  = st { current = i, status = Playing }
    send (writeh st) (Load f)
    return st'

-- | Play the song following the current song, if we're not at the end
-- If we're at the end, and loop mode is on, then loop to the start
-- If the cursor and current are currently the same, continue that.
playNext :: IO ()
playNext = modifyState_ $ \st -> do
    let i   = current st
        j   = cursor  st
        m   = music st
    case () of {_ 
        | i < size st - 1          -- successor
        -> let f     = let fe = m ! (i + 1) in (dname $ folders st !  fdir fe) `joinPathP` (fbase fe)
               st'   = st { current = i + 1
                          , status = Playing
                          , cursor = if i == j then i + 1 else j } 
           in send (writeh st) (Load f) >> return st'

        | mode st == Loop           -- else loop
        -> let  f    = let fe = m ! 0 in (dname $ folders st ! fdir fe) `joinPathP` (fbase fe)
                st'  = st { current = 0, status = Playing
                          , cursor = if i == j then 0 else j } 
           in send (writeh st) (Load f) >> return st'

        | otherwise -> return st    -- else stop
    }

-- | Play a random song
-- refactor with the above code.
playRandom :: IO ()
playRandom = modifyState_ $ \st -> do
    let i   = current st
        j   = cursor  st
        m   = music st
    n <- getStdRandom (randomR (0, size st -1)) -- memoise length m?
    let  f    = let fe = m ! n in (dname $ folders st ! fdir fe) `joinPathP` (fbase fe)
         st'  = st { current = n
                   , status = Playing
                   , cursor = if i == j then n else j }
    send (writeh st) (Load f)
    return st'

-- | Shutdown and exit
quit :: IO ()
quit = shutdown

-- | Move cursor to currently playing song
jumpToPlaying :: IO ()
jumpToPlaying = modifyState_ $ \st -> return st { cursor = (current st) }

-- | Move cursor to first song in next directory (or wrap)
jumpToNextDir :: IO ()
jumpToNextDir = modifyState_ $ \st -> do
    let i   = fdir (music st ! cursor st)
        len = 1 + (snd . bounds $ folders st)
        d   = min (i + 1) (len - 1)
    return st { cursor = dlo ((folders st) ! d) }

-- | Move cursor to first song in next directory (or wrap)
jumpToPrevDir :: IO ()
jumpToPrevDir = modifyState_ $ \st -> do
    let i   = fdir (music st ! cursor st)
        d   = max (i - 1) 0
    return st { cursor = dlo ((folders st) ! d) }

-- | Jump to first element (forwards or backwards) of folder that matches regex
jumpToMatch :: Maybe (String,Bool) -> IO ()
jumpToMatch re = do
    found <- modifyState $ \st -> do
        mre <- case re of   -- work out if we have no pattern, a cached pattern, or a new pattern
                Nothing -> case regex st of
                                Nothing     -> return Nothing
                                Just (r,d)  -> return $ Just (r,d)
                Just (s,d)  -> do v <- regcomp s (regExtended + regIgnoreCase + regNewline)
                                  return $ Just (v,d)

        case mre of 
            Nothing -> return (st,False)    -- no pattern
            Just (p,forwards) -> do

            let fs = folders st
                cur= fdir (music st ! cursor st)
                m  = 1 + (snd . bounds $ folders st)

                loop fn inc n
                    | fn n      = return Nothing
                    | otherwise = P.unsafeUseAsCString (dname $ fs ! n) $ \s -> do
                        v <- regexec p s 0
                        case v of
                            Nothing -> loop fn inc $! inc n
                            Just _  -> return $ Just n

            mi <- if forwards then loop (>=m) (+1) (cur+1) 
                              else loop (<0) (subtract 1) (cur-1)

            let st' = st { regex = Just (p,forwards) }
            return $ case mi of
                Nothing -> (st',False)
                Just i  -> (st' { cursor = dlo (folders st ! i) }, True)

    when (not found) $ putmsg (Plain "No match found.") >> touchState

-- | Show/hide the help window
toggleHelp :: IO ()
toggleHelp = modifyState_ $ \st -> return st { helpVisible = not (helpVisible st) }

-- | Toggle the mode flag
nextMode :: IO ()
nextMode = modifyState_ $ \st -> return st { mode = next (mode st) }
    where 
        next v = if v == maxBound then minBound else succ v

------------------------------------------------------------------------

-- | Add a tree to the playlist
{-
add :: String -> IO ()
add f = do 
    new <- buildTree [P.pack f]
    modifyState_ $ \st -> return st { music = music st ++ [ (n,basenameP n) | n <- new ]
                                    , size = size st + length new }
-}

------------------------------------------------------------------------

-- | Saving the playlist 
writeSt :: IO ()
writeSt = do
    home <- getHome
    let f = home </> ".hmp3db"
    withState $ \st -> do
        let arr1 = music st
            arr2 = folders st
        writeTree f (arr1,arr2) (current st)
    putmsg (Plain $ "Wrote state to " ++ f) >> touchState

-- | Read the playlist back
readSt :: IO (Maybe (FileArray, DirArray, Int))
readSt = do
    home <- getHome
    let f = home </> ".hmp3db"
    b <- doesFileExist f
    if b then liftM Just $ readTree f else return Nothing

getHome :: IO String
getHome = Control.Exception.catch 
    (getRealUserID >>= getUserEntryForID >>= (return . homeDirectory))
    (\_ -> getEnv "HOME")

------------------------------------------------------------------------
-- Editing the minibuffer

-- | Focus the minibuffer
toggleFocus :: IO ()
toggleFocus = modifyState_ $ \st -> return st { miniFocused = not (miniFocused st) }

putmsg :: StringA -> IO ()
putmsg s = unsafeModifyState $ \st -> return st { minibuffer = s }

-- Modify without triggering a refresh
clrmsg :: IO ()
clrmsg = unsafeModifyState $ \s -> return s { minibuffer = empty }
    where empty = Plain []

-- showA :: Show a => a -> IO ()
-- showA = putmsg . Plain . show

warnA :: String -> IO ()
warnA x = putmsg $ Fast (P.pack x) (warnings . style $ config)
