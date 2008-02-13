-- 
-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
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
        seekLeft, seekRight, up, down, pause, nextMode, playNext, playPrev,
        quit, putmsg, clrmsg, toggleHelp, play, jumpToPlaying, jump, {-, add-}
        writeSt, readSt,
        jumpToMatch, jumpToMatchFile,
        toggleFocus, jumpToNextDir, jumpToPrevDir,
        loadConfig,
    ) where

import Prelude hiding (catch)

import Syntax
import Lexer                (parser)
import State
import Style
import Utils
import FastIO               (send,fdToCFile,forceNextPacket)
import Tree hiding (File,Dir)
import qualified Tree (File,Dir)
import qualified UI

import Text.Regex.PCRE.Light
import {-# SOURCE #-} Keymap (keymap)

import qualified Data.ByteString.Char8 as P (ByteString,pack,empty,intercalate,singleton)

import Data.Array               ((!), bounds, Array)
import Data.Maybe               (isJust,fromJust)
import Control.Monad            (liftM, when)
import System.Directory         (doesFileExist,findExecutable)
import System.Environment       (getEnv)
import System.Exit              (ExitCode(ExitSuccess),exitWith)
import System.IO                (hPutStrLn, hGetLine, stderr, hFlush)
import System.IO.Unsafe         (unsafeInterleaveIO)
import System.Process           (waitForProcess)
import System.Time              (getClockTime)
import System.Random.Mersenne

import System.Posix.Process     (exitImmediately)
import System.Posix.User        (getUserEntryForID, getRealUserID, homeDirectory)

import Control.Concurrent
import Control.Exception

import GHC.Handle               (fdToHandle)

#include "config.h"

------------------------------------------------------------------------

start :: Either SerialT [P.ByteString] -> IO ()
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

    silentlyModifyST $ \s -> s
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

    loadConfig

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

            hw          <- fdToHandle (fromIntegral w)  -- so we can use Haskell IO
            ew          <- fdToHandle (fromIntegral e)  -- so we can use Haskell IO
            filep       <- fdToCFile r                   -- so we can use C IO
            mhw         <- newMVar hw
            mew         <- newMVar ew
            mfilep      <- newMVar filep

            modifyST $ \st ->
                       st { mp3pid    = Just pid
                          , writeh    = mhw
                          , errh      = mew
                          , readf     = mfilep
                          , status    = Stopped
                          , info      = Nothing
                          , id3       = Nothing }

            catch (waitForProcess (pid2phdl pid)) (\_ -> return ExitSuccess)
            stop <- getsST doNotResuscitate
            when (stop) $ exitWith ExitSuccess
            warnA $ "Restarting " ++ mpg321 ++ " ..."

------------------------------------------------------------------------

-- | When the editor state has been modified, refresh, then wait
-- for it to be modified again.
refreshLoop :: IO ()
refreshLoop = getsST modified >>= \mvar -> forever $ takeMVar mvar >> UI.refresh

------------------------------------------------------------------------

-- | Once a minute read the clock time
uptimeLoop :: IO ()
uptimeLoop = forever $ do
    threadDelay delay
    now <- getClockTime
    modifyST $ \st -> st { uptime = drawUptime (boottime st) now }
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
    s <- getsST errh >>= readMVar >>= hGetLine
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
    mvar <- getsST readf
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
    (do silentlyModifyST $ \st -> st { doNotResuscitate = True }
        catch writeSt (\_ -> return ())
        withST $ \st -> do
            case mp3pid st of
                Nothing  -> return ()
                Just pid -> do
                    h <- readMVar (writeh st)
                    send h Quit                        -- ask politely
                    waitForProcess $ pid2phdl pid
                    return ())

    `finally`

    (do isXterm <- getsST xterm
        UI.end isXterm
        when (isJust ms) $ hPutStrLn stderr (fromJust ms) >> hFlush stderr
        exitImmediately ExitSuccess)
                -- race. a thread might touch the screen
                -- gets in the way of profiling
--      return ())

------------------------------------------------------------------------
-- 
-- Write incoming messages from the encoder to the global state in the
-- right pigeon hole.
--
handleMsg :: Msg -> IO ()
handleMsg (T _)                = return ()
handleMsg (I i)                = modifyST $ \s -> s { info = Just i }
handleMsg (F (File (Left  _))) = modifyST $ \s -> s { id3 = Nothing }
handleMsg (F (File (Right i))) = modifyST $ \s -> s { id3 = Just i  }

handleMsg (S t) = do
    modifyST $ \s -> s { status  = t }
    when (t == Stopped) $ do   -- transition to next song
        playNext
-- vincenz: Redundant, this is checked in playNext
--        r <- getsST mode
--        if r == Random then playRandom else playNext

handleMsg (R f) = do
    silentlyModifyST $ \st -> st { clock = Just f }
    getsST clockUpdate >>= flip when UI.refreshClock

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
    f <- getsST clock
    case f of
        Nothing -> return ()
        Just g  -> do
            withST $ \st -> do
                h <- readMVar (writeh st)
                send h $ Jump (fn g)
                forceNextPacket         -- don't drop the next Frame.
            silentlyModifyST $ \st -> st { clockUpdate = True }

------------------------------------------------------------------------

-- | Move cursor up or down
up, down :: IO ()
up   = modifySTM $ flip jumpTo (subtract 1)
down = modifySTM $ flip jumpTo (+1)

-- | Move cursor to specified index
jump :: Int -> IO ()
jump i = modifySTM $ flip jumpTo (const i)

-- | Generic jump
jumpTo :: HState -> (Int -> Int) -> IO HState
jumpTo st fn = do
    let l = max 0 (size st - 1)
        i = fn (cursor st)
        n = if i > l then l else if i < 0 then 0 else i
    return st { cursor = n }

------------------------------------------------------------------------

-- | Load and play the song under the cursor
play :: IO ()
play = modifySTM $ \st -> playAtN st (const $ cursor st)

-- | Play a random song
playRandom :: IO ()
playRandom = modifySTM $ \st -> do
    let g = randomGen st
    n' <- random g :: IO Int
    let n = abs n' `mod` (size st -1)
    playAtN st (const n)

-- | Play the song before the current song, if we're not at the beginning
-- If we're at the beginning, and loop mode is on, then loop to the end
-- If we're in random mode, play the next random track
playPrev :: IO ()
playPrev = do
    md <- getsST mode
    if md == Random then playRandom else
      modifySTM $ \st -> do
      let i   = current st
      case () of {_
        | i > 0             -> playAtN st (subtract 1)      -- just the prev track
        | mode st == Loop   -> playAtN st (const (size st - 1))  -- maybe loop
        | otherwise         -> return  st            -- else stop at end
      }

-- | Play the song following the current song, if we're not at the end
-- If we're at the end, and loop mode is on, then loop to the start
-- If we're in random mode, play the next random track
playNext :: IO ()
playNext = do
    md <- getsST mode
    if md == Random then playRandom else
      modifySTM $ \st -> do
      let i   = current st
      case () of {_
        | i < size st - 1   -> playAtN st (+ 1)      -- just the next track
        | mode st == Loop   -> playAtN st (const 0)  -- maybe loop
        | otherwise         -> return  st            -- else stop at end
      }

-- | Generic next song selection
-- If the cursor and current are currently the same, continue that.
playAtN :: HState -> (Int -> Int) -> IO HState
playAtN st fn = do
    let m   = music st
        i   = current st
        fe  = m ! (fn i)
        f   = P.intercalate (P.singleton '/')
                     [(dname $ folders st ! fdir fe),(fbase fe)]
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
pause = withST $ \st -> readMVar (writeh st) >>= flip send Pause

-- | Shutdown and exit
quit :: Maybe String -> IO ()
quit = shutdown

------------------------------------------------------------------------

-- | Move cursor to currently playing song
jumpToPlaying :: IO ()
jumpToPlaying = modifyST $ \st -> st { cursor = (current st) }

-- | Move cursor to first song in next directory (or wrap)
jumpToNextDir, jumpToPrevDir :: IO ()
jumpToNextDir = jumpToDir (\i len -> min (i+1) (len-1))
jumpToPrevDir = jumpToDir (\i _   -> max (i-1) 0)

-- | Generic jump to dir
jumpToDir :: (Int -> Int -> Int) -> IO ()
jumpToDir fn = modifyST $ \st -> if size st == 0 then st else
    let i   = fdir (music st ! cursor st)
        len = 1 + (snd . bounds $ folders st)
        d   = fn i len
    in st { cursor = dlo ((folders st) ! d) }

------------------------------------------------------------------------

--
-- a bit of bounded parametric polymorphism so we can abstract over record selectors
-- in the regex search stuff below
--
class Lookup a       where extract :: a -> FilePathP
instance Lookup Tree.Dir  where extract = dname
instance Lookup Tree.File where extract = fbase

jumpToMatchFile :: Maybe (String,Bool) -> IO ()
jumpToMatchFile re = genericJumpToMatch re k sel
    where k = \st -> (music st, if size st == 0 then -1 else cursor st, size st)
          sel i _ = i

jumpToMatch  :: Maybe (String,Bool) -> IO ()
jumpToMatch     re = genericJumpToMatch re k sel
    where k = \st -> (folders st
                     ,if size st == 0 then -1 else fdir (music st ! cursor st)
                     ,1 + (snd . bounds $ folders st))
          sel i st = dlo (folders st ! i)

genericJumpToMatch :: Lookup a
                   => Maybe (String,Bool)
                   -> (HState -> (Array Int a, Int, Int))
                   -> (Int -> HState -> Int)
                   -> IO ()

genericJumpToMatch re k sel = do
    found <- modifySTM_ $ \st -> do
        let mre = case re of
            -- work out if we have no pattern, a cached pattern, or a new pattern
                Nothing     -> case regex st of
                                Nothing     -> Nothing
                                Just (r,d)  -> Just (r,d)
                Just (s,d)  -> case compileM (P.pack s) [caseless] of
                                Left _      -> Nothing
                                Right v     -> Just (v,d)
        case mre of
            Nothing -> return (st,False)    -- no pattern
            Just (p,forwards) -> do

            let (fs,cur,m) = k st

                loop fn inc n
                    | fn n      = return Nothing
                    | otherwise = do
                        let s = extract (fs ! n)
                        case match p s [] of
                            Nothing -> loop fn inc $! inc n
                            Just _  -> return $ Just n

            mi <- if forwards then loop (>=m) (+1)         (cur+1)
                              else loop (<0)  (subtract 1) (cur-1)

            let st' = st { regex = Just (p,forwards) }
            return $ case mi of
                Nothing -> (st',False)
                Just i  -> (st' { cursor = sel i st }, True)

    when (not found) $ putmsg (Fast (P.pack "No match found.") defaultSty) >> touchST

------------------------------------------------------------------------

-- | Show\/hide the help window
toggleHelp :: IO ()
toggleHelp = modifyST $ \st -> st { helpVisible = not (helpVisible st) }

-- | Focus the minibuffer
toggleFocus :: IO ()
toggleFocus = modifyST $ \st -> st { miniFocused = not (miniFocused st) }

-- | Toggle the mode flag
nextMode :: IO ()
nextMode = modifyST $ \st -> st { mode = next (mode st) }
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
    withST $ \st -> do
        let arr1 = music st
            arr2 = folders st
            idx  = current st
            mde  = mode st
        when (size st > 0) $ writeTree f $ SerialT {
                                            ser_farr = arr1
                                           ,ser_darr = arr2
                                           ,ser_indx = idx
                                           ,ser_mode = mde
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
-- Read styles from ~/.hmp3
--
loadConfig :: IO ()
loadConfig = do
    home <- getHome
    let f = home </> ".hmp3"
    b <- doesFileExist f
    when b $ do     -- otherwise used compiled-in values
        str  <- readFile f
        msty <- catch (readM str >>= return . Just)
                      (const $ warnA "Parse error in ~/.hmp3" >> return Nothing)
        case msty of
            Nothing  -> return ()
            Just rsty -> do
                let sty = buildStyle rsty
                initcolours sty
                modifyST $ \st -> st { config = sty }
    UI.resetui

------------------------------------------------------------------------
-- Editing the minibuffer

putmsg :: StringA -> IO ()
putmsg s = silentlyModifyST $ \st -> st { minibuffer = s }

-- | Modify without triggering a refresh
clrmsg :: IO ()
clrmsg = putmsg (Fast P.empty defaultSty)

--
warnA :: String -> IO ()
warnA x = do
    sty <- getsST config
    putmsg $ Fast (P.pack x) (warnings sty)
