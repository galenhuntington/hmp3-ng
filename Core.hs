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
        quit, clrmsg, toggleHelp, play, jumpToPlaying, jump, {-, add-}
        writeSt, readSt
    ) where

import Prelude hiding (catch)

import POpen
import Syntax
import Lexer
import State
import Style
import Config
import Utils
import FastIO       ( fdToCFile )
import Tree
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

import Foreign.C.Types          ( CFile )
import Foreign.Ptr              ( Ptr )

import System.Directory
import System.Posix.User        ( getUserEntryForID, getRealUserID, homeDirectory )

import GHC.Base
import GHC.Handle
import GHC.Exception hiding     ( catch )
import GHC.IOBase               ( unsafeInterleaveIO )

#include "config.h"

------------------------------------------------------------------------

start :: Either (FileArray,DirArray) [P.FastString] -> IO ()
start ms = 
    Control.Exception.handle
        (\e -> do if isOK e then return ()
                            else do shutdown
                                    hPutStrLn stderr ("hmp3:start: " ++ show e)
                                    exitWith (ExitFailure 1)) $ do

        -- initialise curses
        UI.start

        -- parse args
        (ds,fs) <- case ms of
                    Left (fs',ds') -> return (ds',fs')
                    Right roots    -> buildTree roots

        -- fork process first. could fail. pass handles over to threads
        (r,w,pid) <- popen (MPG321 :: String) ["-R","-"]

        hw <- fdToHandle (unsafeCoerce# w)  -- so we can use Haskell IO

        now <- getClockTime

        modifyState_ $ \s -> return s 
            { mp3pid    = pid
            , music     = fs
            , folders   = ds
            , size      = 1 + (snd . bounds $ fs)
            , current   = 0
            , cursor    = 0
            , uptime    = drawUptime now now
            , boottime  = now
            , pipe      = Just hw } 

        -- fork some threads
        t   <- forkIO inputLoop
        t'  <- forkIO refreshLoop
        t'' <- forkIO clockLoop
        t'''<- forkIO uptimeLoop
        modifyState_ $ \s -> return s { threads = [t,t',t'',t'''] } 

        -- start the first song
        play

        -- start the main loop
        filep <- fdToCFile r
        run filep
        shutdown

    where
        isOK (ExitException ExitSuccess) = True
        isOK _ = False
 
        -- | When the editor state has been modified, refresh, then wait
        -- for it to be modified again.
        refreshLoop :: IO ()
        refreshLoop = repeatM_ $ handle (\e -> warnA e >> return ()) $ do
                takeMVar modified
                catchJust ioErrors UI.refresh warnA
 
        -- | Once a minute read the clock time
        uptimeLoop :: IO ()
        uptimeLoop = repeatM_ $ handle (\e -> warnA e >> return ()) $ do
                threadDelay delay 
                now <- getClockTime 
                modifyState_ $ \st -> do
                    let t = drawUptime (boottime st) now
                 -- hPutStrLn stderr ("TIME " ++ show t)
                    return st { uptime = t }
            where
                delay = 60 * 1000 * 1000 -- 1 minute

        -- | Once each second, wake up a and redraw the clock
        clockLoop :: IO ()
        clockLoop = repeatM_ $ handle (\e -> warnA e >> return ()) $ do
                threadDelay delay 
            --  hPutStrLn stderr "CLOCK"
                catchJust ioErrors UI.refreshClock warnA
            where
                delay = 500 * 1000 -- 0.5 second

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
--
-- Expensive. Should use packed strings.
--
run :: Ptr CFile -> IO ()
run p = do
    handle (\e -> warnA e >> return ()) $ do
        res <- parser p
        case res of
            Right m -> handleMsg m
            Left e  -> warnA e  -- error from pipe
        run p

-- | Close most things. Important to do all the jobs:
shutdown :: IO ()
shutdown = handle (\e -> hPutStrLn stderr (show e) >> return ()) $ 
    modifyState_ $ \st -> do
        UI.end
        let pid = mp3pid st
            tds = threads st

        handle (\_ -> return ()) $ do
            send (pipe st) Quit                         -- ask politely
            waitForProcess $ unsafeCoerce# pid          -- wait
            return ()

--      Should check if it's still running
--      handle (\_ -> return ()) $
--          signalProcess sigTERM (unsafeCoerce# pid)   -- just kill it

        flip mapM_ tds $ \t -> 
            catch (killThread t) (\_ -> return ())      -- and kill threads

        return st { mp3pid = (-1){-?-}, threads = [] }

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
                send (pipe st) $ Jump (max 0 (fr-400))
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
                    send (pipe st) $ Jump (fr + (min 400 (framesLeft g)))
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
pause = withState $ \st -> send (pipe st) Pause

-- | Load and play the song under the cursor
play :: IO ()
play = modifyState_ $ \st -> do
    let i     = cursor st
        m     = music st
        (f,_) = m ! i
        st'   = st { current = i, status = Playing }
    send (pipe st) (Load f)
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
        -> let (f,_) = m ! (i + 1)
               st'   = st { current = i + 1
                          , status = Playing
                          , cursor = if i == j then i + 1 else j } 
           in send (pipe st) (Load f) >> return st'

        | mode st == Loop           -- else loop
        -> let (f,_) = m ! 0
               st'   = st { current = 0, status = Playing
                          , cursor = if i == j then 0 else j } 
           in send (pipe st) (Load f) >> return st'

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
    let (f,_) = m ! n
        st'   = st { current = n
                   , status = Playing
                   , cursor = if i == j then n else j }
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
        writeTree f (arr1,arr2)
    putmsg (Plain $ "Wrote state to " ++ f) >> touchState

-- | Read the playlist back
readSt :: IO (Maybe (FileArray, DirArray))
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

