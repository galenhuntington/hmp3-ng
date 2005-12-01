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

import Syntax
import Lexer                    (parser)
import State
import Style                    (StringA(..), warnings, defaultSty)
import Config                   (config, Config(style, keymap))
import Utils
import FastIO                   (fdToCFile, joinPathP)
import Tree hiding (File)
import Regex
import qualified UI             (start, refreshClock, refresh, getKey, end)

import qualified Data.FastPackedString as P

import Data.Array               ((!), bounds)
import Data.Maybe               (isJust,fromJust)

import Control.Monad            (liftM, when)

import System.Directory         (doesFileExist,findExecutable)
import System.Environment       (getEnv)
import System.Exit              (ExitCode(..),exitWith)
import System.IO                (hPutStrLn, hGetLine, stderr, hFlush)
import System.Process           (waitForProcess)
import System.Random            (getStdRandom, randomR)
import System.Time              (getClockTime)

import System.Posix.Process     (exitImmediately)
import System.Posix.User        (getUserEntryForID, getRealUserID, homeDirectory)

import Control.Concurrent
import Control.Exception

import GHC.Handle               (fdToHandle)
import GHC.IOBase               (unsafeInterleaveIO)

#include "config.h"

------------------------------------------------------------------------

start :: Either (FileArray,DirArray,Int) [P.FastString] -> IO ()
start ms = Control.Exception.handle (\e -> shutdown (Just (show e))) $ do

    t0 <- forkIO mpgLoop    -- start this off early, to give mpg321 a time to settle

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
    t1 <- forkIO mpgInput
    t2 <- forkIO refreshLoop
    t3 <- forkIO clockLoop
    t4 <- forkIO uptimeLoop
    t5 <- forkIO errorLoop
    silentlyModifyState $ \s -> return s { threads = [t0,t1,t2,t3,t4,t5] } 

    when (0 <= (snd . bounds $ fs)) play -- start the first song

    run         -- won't restart if this fails!

------------------------------------------------------------------------

-- | Uniform loop and thread handler (subtle, and requires exitImmediately)
forever :: IO () -> IO ()
forever fn = catch (repeatM_ fn) handler
    where
        handler :: Exception -> IO ()
        handler e =
            when (not.exitTime $ e) $
                (warnA . show) e >> (forever fn)        -- reopen the catch

-- | Generic handler
-- For profiling, make sure to return True for anything:
exitTime :: Exception -> Bool
exitTime e | isJust . ioErrors $ e   = False -- ignore
           | isJust . errorCalls $ e = False -- ignore
           | isJust . userErrors $ e = False -- ignore
           | otherwise               = True
-- exitTime _ = True

------------------------------------------------------------------------

-- | Process loop, launch mpg321, set the handles in the state
-- and then wait for the process to die. If it does, restart it.
--
-- If we're unable to start at all, we should say something sensible
-- For example, if we can't start it two times in a row, perhaps give up?
--
mpgLoop :: IO ()
mpgLoop = forever $ do
    mmpg <- findExecutable (MPG321 :: String)
    case mmpg of
      Nothing     -> quit (Just $ "Cannot find " ++ MPG321 ++ " in path")
      Just mpg321 -> do 

        -- if we're never able to start mpg321, do something sensible
        mv <- catch (popen (mpg321 :: String) ["-R","-"] >>= return . Just)
                    (\e -> do warnA ("Unable to start " ++ MPG321 ++ ": " ++ show e)
                              return Nothing)
        case mv of
            Nothing -> threadDelay (1000 * 500) >> mpgLoop
            Just (r,w,e,pid) -> do

            hw          <- fdToHandle (fdToInt w)  -- so we can use Haskell IO
            ew          <- fdToHandle (fdToInt e)  -- so we can use Haskell IO
            filep       <- fdToCFile r                   -- so we can use C IO
            mhw         <- newMVar hw
            mew         <- newMVar ew
            mfilep      <- newMVar filep

            modifyState_ $ \st ->
                    return st { mp3pid    = Just pid
                              , writeh    = mhw
                              , errh      = mew
                              , readf     = mfilep 
                              , status    = Stopped
                              , info      = Nothing
                              , id3       = Nothing }
          
            catch (waitForProcess (pid2phdl pid)) (\_ -> return ExitSuccess)
            stop <- readState doNotResuscitate
            when (stop) $ exitWith ExitSuccess
            warnA $ "Restarting " ++ mpg321 ++ " ..."

------------------------------------------------------------------------

-- | When the editor state has been modified, refresh, then wait
-- for it to be modified again.
refreshLoop :: IO ()
refreshLoop = forever $ takeMVar modified >> UI.refresh

------------------------------------------------------------------------

-- | Once a minute read the clock time
uptimeLoop :: IO ()
uptimeLoop = forever $ do
    threadDelay delay 
    now <- getClockTime 
    modifyState_ $ \st -> return st { uptime = drawUptime (boottime st) now }
  where
    delay = 60 * 1000 * 1000 -- 1 minute

------------------------------------------------------------------------

-- | Once each half second, wake up a and redraw the clock
clockLoop :: IO ()
clockLoop = forever $ threadDelay delay >> UI.refreshClock
  where
    delay = 500 * 1000 -- 0.5 second

------------------------------------------------------------------------

-- | Handle, and display errors produced by mpg321
errorLoop :: IO ()
errorLoop = forever $ do
    s <- readState errh >>= readMVar >>= hGetLine
    if s == "No default libao driver available."
        then quit $ Just $ s ++ " Perhaps another instance of hmp3 is running?"
        else warnA s

------------------------------------------------------------------------

-- | Handle messages arriving over a pipe from the decoder process. When
-- shutdown kills the other end of the pipe, hGetLine will fail, so we
-- take that chance to exit.
--
mpgInput :: IO ()
mpgInput = forever $ do
    mvar <- readState readf
    fp   <- readMVar mvar
    res  <- parser fp
    case res of
        Right m -> handleMsg m
        Left e  -> (warnA.show) e  -- error from pipe

------------------------------------------------------------------------

-- | The main thread: handle keystrokes fed to us by curses
run :: IO ()
run = forever $ sequence_ . (keymap config) =<< getKeys
  where
    getKeys = unsafeInterleaveIO $ do
            c  <- UI.getKey
            cs <- getKeys
            return (c:cs) -- A lazy list of curses keys

------------------------------------------------------------------------

-- | Close most things. Important to do all the jobs:
shutdown :: Maybe String -> IO ()
shutdown ms = 
    (do silentlyModifyState $ \st -> return st { doNotResuscitate = True }
        catch writeSt (\_ -> return ())
        withState $ \st -> do
            case mp3pid st of
                Nothing  -> return ()
                Just pid -> do
                    h <- readMVar (writeh st)
                    send h Quit                        -- ask politely
                    waitForProcess $ pid2phdl pid
                    return ())

    `finally` 

    (do isXterm <- readState xterm
        UI.end isXterm
        when (isJust ms) $ hPutStrLn stderr (fromJust ms) >> hFlush stderr
        exitImmediately ExitSuccess)    -- race. a thread might touch the screen
--      return ())

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
    x <- tryTakeMVar clockModified   -- immediate update
    when (isJust x) UI.refreshClock

------------------------------------------------------------------------
--
-- Basic operations
--

-- | Seek backward in song
seekLeft :: IO ()
seekLeft = seek $ \g -> max 0 (currentFrame g - 400)

-- | Seek forward in song
seekRight :: IO ()
seekRight = seek $ \g -> currentFrame g + (min 400 (framesLeft g))

-- | Generic seek
seek :: (Frame -> Int) -> IO ()
seek fn = do
    f <- readClock id
    case f of 
        Nothing -> return ()
        Just g  -> do
            withState $ \st -> do
                h <- readMVar (writeh st)
                send h $ Jump (fn g)
            tryPutMVar clockModified () -- touch the modified MVar
            return ()

------------------------------------------------------------------------

-- | Move cursor up
up :: IO ()
up = modifyState_ $ \st -> do
    let i = cursor st
    return $ if i > 0 then st { cursor = i - 1 } else st

-- | Move cursor down list
down :: IO ()
down = modifyState_ $ \st -> do
    let i = cursor st
        l = max 0 (size st - 1)
    return $ if i == l then st else st { cursor = i + 1 }

-- | Move cursor to specified index
jump :: Int -> IO ()
jump i = modifyState_ $ \st -> do
    let l = max 0 (size st - 1)
        n = if i > l then l else if i < 0 then 0 else i
    return st { cursor = n }

------------------------------------------------------------------------

-- | Toggle pause on the current song
pause :: IO ()
pause = withState $ \st -> do
    h <- readMVar (writeh st)
    send h Pause

-- | Load and play the song under the cursor
play :: IO ()
play = modifyState_ $ \st -> do
    let i    = cursor st
        m    = music st
        f    = let fe = m ! i in (dname $ folders st ! fdir fe) `joinPathP` (fbase fe)
        st'  = st { current = i, status = Playing }
    h <- readMVar (writeh st)
    send h (Load f)
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
           in do h <- readMVar (writeh st)
                 send h (Load f) >> return st'

        | mode st == Loop           -- else loop
        -> let  f    = let fe = m ! 0 in (dname $ folders st ! fdir fe) `joinPathP` (fbase fe)
                st'  = st { current = 0, status = Playing
                          , cursor = if i == j then 0 else j } 
           in do h <- readMVar (writeh st)
                 send h (Load f) >> return st'

        | otherwise -> return st    -- else stop
    }

-- | Play a random song
-- refactor with the above code.
playRandom :: IO ()
playRandom = modifyState_ $ \st -> do
    let i   = current st
        j   = cursor  st
        m   = music st
    n <- getStdRandom (randomR (0, max 0 (size st -1))) -- memoise length m?
    let  f    = let fe = m ! n in (dname $ folders st ! fdir fe) `joinPathP` (fbase fe)
         st'  = st { current = n
                   , status = Playing
                   , cursor = if i == j then n else j }
    h <- readMVar (writeh st)
    send h (Load f)
    return st'

------------------------------------------------------------------------

-- | Shutdown and exit
quit :: Maybe String -> IO ()
quit = shutdown

-- | Move cursor to currently playing song
jumpToPlaying :: IO ()
jumpToPlaying = modifyState_ $ \st -> return st { cursor = (current st) }

-- | Move cursor to first song in next directory (or wrap)
jumpToNextDir :: IO ()
jumpToNextDir = modifyState_ $ \st -> return $ if size st == 0 then st else
    let i   = fdir (music st ! cursor st)
        len = 1 + (snd . bounds $ folders st)
        d   = min (i + 1) (len - 1)
    in st { cursor = dlo ((folders st) ! d) }

-- | Move cursor to first song in next directory (or wrap)
jumpToPrevDir :: IO ()
jumpToPrevDir = modifyState_ $ \st -> return $ if size st == 0 then st else
    let i   = fdir (music st ! cursor st)
        d   = max (i - 1) 0
    in st { cursor = dlo ((folders st) ! d) }

------------------------------------------------------------------------

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
                cur= if size st == 0 then -1 else fdir (music st ! cursor st)
                m  = 1 + (snd . bounds $ folders st)

                loop fn inc n
                    | fn n      = return Nothing
                    | otherwise = P.unsafeUseAsCString (dname $ fs ! n) $ \s -> do
                        v <- regexec p s 0
                        case v of
                            Nothing -> loop fn inc $! inc n
                            Just _  -> return $ Just n

            mi <- if forwards then loop (>=m) (+1)         (cur+1) 
                              else loop (<0)  (subtract 1) (cur-1)

            let st' = st { regex = Just (p,forwards) }
            return $ case mi of
                Nothing -> (st',False)
                Just i  -> (st' { cursor = dlo (folders st ! i) }, True)

    when (not found) $ putmsg (Fast (P.pack "No match found.") defaultSty) >> touchState

------------------------------------------------------------------------

-- | Show/hide the help window
toggleHelp :: IO ()
toggleHelp = modifyState_ $ \st -> return st { helpVisible = not (helpVisible st) }

-- | Toggle the mode flag
nextMode :: IO ()
nextMode = modifyState_ $ \st -> return st { mode = next (mode st) }
    where 
        next v = if v == maxBound then minBound else succ v

------------------------------------------------------------------------

-- | Saving the playlist 
-- Only save if there's something to save. Should preven dbs being wiped
-- if curses crashes before the state is read.
writeSt :: IO ()
writeSt = do
    home <- getHome
    let f = home </> ".hmp3db"
    withState $ \st -> do
        let arr1 = music st
            arr2 = folders st
        when (size st > 0) $ writeTree f (arr1,arr2) (current st)

-- | Read the playlist back
readSt :: IO (Maybe (FileArray, DirArray, Int))
readSt = do
    home <- getHome
    let f = home </> ".hmp3db"
    b <- doesFileExist f
    if b then liftM Just $! readTree f else return Nothing

-- | Find a user's home in a canonical sort of way
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
putmsg s = silentlyModifyState $ \st -> return st { minibuffer = s }

-- Modify without triggering a refresh
clrmsg :: IO ()
clrmsg = silentlyModifyState $ \s -> return s { minibuffer = empty }
    where empty = Fast P.empty defaultSty

warnA :: String -> IO ()
warnA x = putmsg $ Fast (P.pack x) (warnings . style $ config)

