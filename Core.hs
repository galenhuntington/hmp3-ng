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
import Lexer                (parser)
import State
import Style                (StringA(..), warnings, defaultSty, UIStyle)
import Utils                ((</>),popen,pid2phdl,fdToInt,repeatM_,drawUptime)
import FastIO               (send,fdToCFile,joinPathP,forceNextPacket)
import Tree hiding (File)
import Regex
import qualified UI         (start, refreshClock, refresh, getKey, end)

import {-# SOURCE #-} Keymap (keymap)

import qualified Data.FastPackedString as P (unsafeUseAsCString,pack,empty,FastString)

import Data.Array               ((!), bounds)
import Data.Maybe               (isJust,fromJust)
import Control.Monad            (liftM, when)
import System.Directory         (doesFileExist,findExecutable)
import System.Environment       (getEnv)
import System.Exit              (ExitCode(ExitSuccess),exitWith)
import System.IO                (hPutStrLn, hGetLine, stderr, hFlush)
import System.IO.Unsafe         (unsafeInterleaveIO)
import System.Process           (waitForProcess)
import System.Random            (getStdRandom, randomR)
import System.Time              (getClockTime)

import System.Posix.Process     (exitImmediately)
import System.Posix.User        (getUserEntryForID, getRealUserID, homeDirectory)

import Control.Concurrent
import Control.Exception

import GHC.Handle               (fdToHandle)

#include "config.h"

------------------------------------------------------------------------

start :: Either SerialT [P.FastString] -> IO ()
start ms = Control.Exception.handle (\e -> shutdown (Just (show e))) $ do

    t0 <- forkIO mpgLoop    -- start this off early, to give mpg321 a time to settle

    c <- UI.start -- initialise curses

    (ds,fs,i,m)   -- construct the state
        <- case ms of
           Right roots -> do (a,b) <- buildTree roots 
                             return (a,b,0,Normal)

           Left st     -> return (ser_darr st
                                 ,ser_farr st
                                 ,ser_indx st
                                 ,ser_mode st)

    now   <- getClockTime

    -- fork some threads
    t1 <- forkIO mpgInput
    t2 <- forkIO refreshLoop
    t3 <- forkIO clockLoop
    t4 <- forkIO uptimeLoop
    t5 <- forkIO errorLoop

    silentlyModifyState $ \s -> s 
        { music        = fs
        , folders      = ds
        , size         = 1 + (snd . bounds $ fs)
        , cursor       = i
        , current      = i
        , mode         = m
        , uptime       = drawUptime now now
        , boottime     = now 
        , config       = c
        , threads      = [t0,t1,t2,t3,t4,t5] }

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
run = forever $ sequence_ . keymap =<< getKeys
  where
    getKeys = unsafeInterleaveIO $ do
            c  <- UI.getKey
            cs <- getKeys
            return (c:cs) -- A lazy list of curses keys

------------------------------------------------------------------------

-- | Close most things. Important to do all the jobs:
shutdown :: Maybe String -> IO ()
shutdown ms = 
    (do silentlyModifyState $ \st -> st { doNotResuscitate = True }
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
handleMsg (I i)                = modifyState_ $ \s -> return s { info = Just i }
handleMsg (F (File (Left  _))) = modifyState_ $ \s -> return s { id3 = Nothing }
handleMsg (F (File (Right i))) = modifyState_ $ \s -> return s { id3 = Just i  }

handleMsg (S t) = do
    modifyState_ $ \s -> return s { status  = t }
    when (t == Stopped) $ do   -- transition to next song
        r <- readState mode
        if r == Random then playRandom else playNext

handleMsg (R f) = do
    modifyClock $ \_ -> return $ Just f
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
                forceNextPacket         -- don't drop the next Frame.
            tryPutMVar clockModified () -- touch the modified MVar
            return ()

------------------------------------------------------------------------

-- | Move cursor up or down
up, down :: IO ()
up   = modifyState_ $ flip jumpTo (subtract 1)
down = modifyState_ $ flip jumpTo (+1)

-- | Move cursor to specified index
jump :: Int -> IO ()
jump i = modifyState_ $ flip jumpTo (const i)

-- | Generic jump
jumpTo :: State -> (Int -> Int) -> IO State
jumpTo st fn = do
    let l = max 0 (size st - 1)
        i = fn (cursor st)
        n = if i > l then l else if i < 0 then 0 else i
    return st { cursor = n }

------------------------------------------------------------------------

-- | Load and play the song under the cursor
play :: IO ()
play = modifyState_ $ \st -> playAtN st (const $ cursor st)

-- | Play a random song
playRandom :: IO ()
playRandom = modifyState_ $ \st -> do
    n <- getStdRandom (randomR (0, max 0 (size st -1)))
    playAtN st (const n)

-- | Play the song following the current song, if we're not at the end
-- If we're at the end, and loop mode is on, then loop to the start
-- If we're in random mode, play the next random track
playNext :: IO ()
playNext = do
    md <- readState mode
    if md == Random then playRandom else
      modifyState_ $ \st -> do
      let i   = current st
      case () of {_
        | i < size st - 1   -> playAtN st (+ 1)      -- just the next track
        | mode st == Loop   -> playAtN st (const 0)  -- maybe loop
        | otherwise         -> return  st            -- else stop at end
      }

-- | Generic next song selection
-- If the cursor and current are currently the same, continue that.
playAtN :: State -> (Int -> Int) -> IO State
playAtN st fn = do
    let m   = music st
        i   = current st
        fe  = m ! (fn i)
        f   = (dname $ folders st ! fdir fe) `joinPathP` (fbase fe)
        j   = cursor  st
        st' = st { current = fn i
                 , status  = Playing
                 , cursor  = if i == cursor st then fn i else j }
    h <- readMVar (writeh st)
    send h (Load f)
    return st'

------------------------------------------------------------------------

-- | Toggle pause on the current song
pause :: IO ()
pause = withState $ \st -> readMVar (writeh st) >>= flip send Pause

-- | Shutdown and exit
quit :: Maybe String -> IO ()
quit = shutdown

------------------------------------------------------------------------

-- | Move cursor to currently playing song
jumpToPlaying :: IO ()
jumpToPlaying = modifyState_ $ \st -> return st { cursor = (current st) }

-- | Move cursor to first song in next directory (or wrap)
-- | Move cursor to first song in next directory (or wrap)
jumpToNextDir, jumpToPrevDir :: IO ()
jumpToNextDir = jumpToDir (\i len -> min (i+1) (len-1))
jumpToPrevDir = jumpToDir (\i _   -> max (i-1) 0)

-- | Generic jump to dir
jumpToDir :: (Int -> Int -> Int) -> IO ()
jumpToDir fn = modifyState_ $ \st -> return $ if size st == 0 then st else
    let i   = fdir (music st ! cursor st)
        len = 1 + (snd . bounds $ folders st)
        d   = fn i len
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

-- | Focus the minibuffer
toggleFocus :: IO ()
toggleFocus = modifyState_ $ \st -> return st { miniFocused = not (miniFocused st) }

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
        when (size st > 0) $ writeTree f $ SerialT {
                                            ser_farr = arr1
                                           ,ser_darr = arr2
                                           ,ser_indx = current st
                                           ,ser_mode = mode st
                                          }

-- | Read the playlist back
readSt :: IO (Maybe SerialT)
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

putmsg :: StringA -> IO ()
putmsg s = silentlyModifyState $ \st -> st { minibuffer = s }

-- Modify without triggering a refresh
clrmsg :: IO ()
clrmsg = putmsg (Fast P.empty defaultSty)

--
warnA :: String -> IO ()
warnA x = do 
    sty <- readState config
    putmsg $ Fast (P.pack x) (warnings sty)

