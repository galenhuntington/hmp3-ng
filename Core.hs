{-# LANGUAGE CPP, AllowAmbiguousTypes #-}

-- 
-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2008, 2019-2021 Galen Huntington
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
        quit, putmsg, clrmsg, toggleHelp, play, playCur,
        jumpToPlaying, jump, jumpRel,
        upPage, downPage,
        seekStart,
        blacklist,
        writeSt, readSt,
        jumpToMatch, jumpToMatchFile,
        toggleFocus, jumpToNextDir, jumpToPrevDir,
        loadConfig,
        FileListSource,
    ) where

import Base

import Syntax
import Lexer                (parser)
import State
import Style
import FastIO               (send, FiltHandle(..), newFiltHandle)
import Tree hiding (File,Dir)
import qualified Tree (File,Dir)
import qualified UI

import Text.Regex.PCRE.Light
import {-# SOURCE #-} Keymap (keymap)

import qualified Data.ByteString.Char8 as P

import Data.Array               ((!), bounds, Array)
import System.Directory         (doesFileExist,findExecutable)
import System.IO                (hPutStrLn, hGetLine, stderr, hFlush)
import System.Process           (runInteractiveProcess, waitForProcess)
import System.Clock             (getTime, TimeSpec(..), Clock(..), diffTimeSpec)
import System.Random            (randomIO)
import System.FilePath          ((</>))

import System.Posix.Process     (exitImmediately)
import System.Posix.User        (getUserEntryForID, getRealUserID, homeDirectory)

type FileListSource = Either SerialT [ByteString]


mp3Tool :: String
mp3Tool =
#ifdef MPG123
    "mpg123"
#else
    "mpg321"
#endif

------------------------------------------------------------------------

start :: Bool -> FileListSource -> IO ()
start playNow ms = handle @SomeException (shutdown . Just . show) do

    t0 <- forkIO mpgLoop    -- start this off early, to give mpg321 time to settle

    c <- UI.start -- initialise curses

    (ds,fs,i,m)   -- construct the state
        <- case ms of
           Right roots -> do Tree a b <- buildTree roots
                             pure (a,b,0,Normal)

           Left st     -> pure (ser_darr st
                               ,ser_farr st
                               ,ser_indx st
                               ,ser_mode st)

    now <- getTime Monotonic

    -- fork some threads
    t1 <- forkIO $ mpgInput readf
    t2 <- forkIO refreshLoop
    t3 <- forkIO clockLoop
    t4 <- forkIO uptimeLoop
    t5 <- forkIO
        -- mpg321 uses stderr for @F messages
        $ if mp3Tool == "mpg321" then mpgInput errh else errorLoop

    silentlyModifyST $ \s -> s
        { music        = fs
        , folders      = ds
        , size         = 1 + (snd . bounds $ fs)
        , cursor       = i
        , current      = i
        , mode         = m
        , uptime       = showUptime now now
        , boottime     = now
        , config       = c
        , threads      = [t0,t1,t2,t3,t4,t5] }

    loadConfig

    when (0 <= (snd . bounds $ fs)) play -- start the first song
    when (not playNow) pause

    run         -- won't restart if this fails!

------------------------------------------------------------------------

-- | Uniform loop and thread handler (subtle, and requires exitImmediately)
runForever :: IO () -> IO ()
runForever fn = catch (forever fn) handler
    where
        handler :: SomeException -> IO ()
        handler e =
            unless (exitTime e) $
                (warnA . show) e >> runForever fn        -- reopen the catch

-- | Generic handler
-- I don't know why these are ignored, but preserving old logic.
-- For profiling, make sure to return True for anything:
exitTime :: SomeException -> Bool
exitTime e | is @IOException e = False -- ignore
           | is @ErrorCall e   = False -- ignore
           -- "user errors" were caught before, but are no longer a thing
           | otherwise         = True
    where is :: forall e. Exception e => SomeException -> Bool
          is = isJust . fromException @e

------------------------------------------------------------------------

-- | Process loop, launch mpg321, set the handles in the state
-- and then wait for the process to die. If it does, restart it.
--
-- If we're unable to start at all, we should say something sensible
-- For example, if we can't start it two times in a row, perhaps give up?
--
mpgLoop :: IO ()
mpgLoop = runForever do
    mmpg <- findExecutable mp3Tool
    case mmpg of
      Nothing     -> quit (Just $ "Cannot find " ++ mp3Tool ++ " in path")
      Just mpg321 -> do

        -- if we're never able to start mpg321, do something sensible
        --   TODO no need for this Maybe unpacking, just catch and rerun loop
        mv <- catch (pure <$> runInteractiveProcess mpg321 ["-R","-"] Nothing Nothing)
                    (\ (e :: SomeException) ->
                           do warnA ("Unable to start " ++ mp3Tool ++ ": " ++ show e)
                              pure Nothing)
        case mv of
            Nothing -> threadDelay (1000 * 500) >> mpgLoop
            Just (hw, r, e, pid) -> do

            mhw         <- newMVar hw
            mew         <- newMVar =<< newFiltHandle e
            mfilep      <- newMVar =<< newFiltHandle r

            modifyST $ \st ->
                       st { mp3pid    = Just pid
                          , writeh    = mhw
                          , errh      = mew
                          , readf     = mfilep
                          , status    = Stopped
                          , info      = Nothing
                          , id3       = Nothing }

            catch @SomeException (void $ waitForProcess pid) (\_ -> pure ())
            stop <- getsST doNotResuscitate
            when stop exitSuccess
            warnA $ "Restarting " ++ mpg321 ++ " ..."

------------------------------------------------------------------------

-- | When the editor state has been modified, refresh, then wait
-- for it to be modified again.
refreshLoop :: IO ()
refreshLoop = do
    mvar <- getsST modified
    runForever $ takeMVar mvar >> UI.refresh

------------------------------------------------------------------------

-- | The clock ticks once per minute, but check more often in case of drift.
uptimeLoop :: IO ()
uptimeLoop = runForever $ do
    threadDelay delay
    now <- getTime Monotonic
    modifyST $ \st -> st { uptime = showUptime (boottime st) now }
  where
    delay = 5 * 1000 * 1000 -- refresh every 5 seconds

------------------------------------------------------------------------

showUptime :: TimeSpec -> TimeSpec -> ByteString
showUptime before now
    | hs == 0 = P.pack $ printf "%dm" m
    | d == 0  = P.pack $ printf "%dh%02dm" h m
    | True    = P.pack $ printf "%dd%02dh%02dm" d h m
    where
        s      = sec $ diffTimeSpec before now
        ms     = quot s 60
        (hs,m) = quotRem ms 60
        (d,h)  = quotRem hs 24

------------------------------------------------------------------------

-- | Once each half second, wake up a and redraw the clock
clockLoop :: IO ()
clockLoop = runForever $ threadDelay delay >> UI.refreshClock
  where
    delay = 500 * 1000 -- 0.5 second

------------------------------------------------------------------------

-- | Handle, and display errors produced by mpg321
errorLoop :: IO ()
errorLoop = runForever $ do
    s <- getsST errh >>= readMVar >>= hGetLine . filtHandle
    if s == "No default libao driver available."
        then quit $ Just $ s ++ " Perhaps another instance of hmp3 is running?"
        else warnA s

------------------------------------------------------------------------

-- | Handle messages arriving over a pipe from the decoder process. When
-- shutdown kills the other end of the pipe, hGetLine will fail, so we
-- take that chance to exit.
--
mpgInput :: (HState -> MVar FiltHandle) -> IO ()
mpgInput field = runForever $ do
    mvar <- getsST field
    fp   <- readMVar mvar
    res  <- parser fp
    case res of
        Right m       -> handleMsg m
        Left (Just e) -> (warnA.show) e
        _             -> pure ()

------------------------------------------------------------------------

-- | The main thread: handle keystrokes fed to us by curses
run :: IO ()
run = runForever $ sequence_ . keymap =<< getKeys
  where
    getKeys = unsafeInterleaveIO $ do
            c  <- UI.getKey
            cs <- getKeys
            pure (c:cs) -- A lazy list of curses keys

------------------------------------------------------------------------

-- | Close most things. Important to do all the jobs:
shutdown :: Maybe String -> IO ()
shutdown ms =
    do  silentlyModifyST $ \st -> st { doNotResuscitate = True }
        handle @SomeException (\_ -> pure ()) writeSt
        withST $ \st -> do
            case mp3pid st of
                Nothing  -> pure ()
                Just pid -> do
                    h <- readMVar (writeh st)
                    send h Quit                        -- ask politely
                    waitForProcess pid
                    pure ()

    `finally`

    do  isXterm <- getsST xterm
        UI.end isXterm
        when (isJust ms) $ hPutStrLn stderr (fromJust ms) >> hFlush stderr
        exitImmediately ExitSuccess

------------------------------------------------------------------------
-- 
-- Write incoming messages from the encoder to the global state in the
-- right pigeon hole.
--
handleMsg :: Msg -> IO ()
handleMsg (T _)                = pure ()
handleMsg (I i)                = modifyST $ \s -> s { info = Just i }
handleMsg (F (File (Left  _))) = modifyST $ \s -> s { id3 = Nothing }
handleMsg (F (File (Right i))) = modifyST $ \s -> s { id3 = Just i  }

handleMsg (S t) = do
    modifyST $ \s -> s { status  = t }
    when (t == Stopped) playNext   -- transition to next song

handleMsg (R f) = do
    silentlyModifyST \st -> st { clock = Just f }
    getsST clockUpdate >>= flip when UI.refreshClock

------------------------------------------------------------------------
--
-- Basic operations
--

-- | Seek backward in song
seekLeft :: IO ()
seekLeft = seek \g -> max 0 (currentFrame g - 400)

-- | Seek forward in song
seekRight :: IO ()
seekRight = seek \g -> currentFrame g + min 400 (framesLeft g)

seekStart :: IO ()
seekStart = seek $ const 0


-- | Generic seek
seek :: (Frame -> Int) -> IO ()
seek fn = do
    f <- getsST clock
    case f of
        Nothing -> pure ()
        Just g  -> do
            withST $ \st -> do
                h <- readMVar (writeh st)
                send h $ Jump (fn g)
                forceNextPacket         -- don't drop the next Frame.
            silentlyModifyST $ \st -> st { clockUpdate = True }

page :: Int -> IO ()
page dir = do
    (sz, _) <- UI.screenSize
    modifySTM $ flip jumpTo (+ dir*(sz-5))
upPage, downPage :: IO ()
upPage   = page (-1)
downPage = page ( 1)


------------------------------------------------------------------------

-- | Move cursor up or down
up, down :: IO ()
up   = modifySTM $ flip jumpTo (subtract 1)
down = modifySTM $ flip jumpTo (+1)

-- | Move cursor to specified index
jump :: Int -> IO ()
jump i = modifySTM $ flip jumpTo (const i)

-- | Jump to relative place, 0 to 1.
jumpRel :: Float -> IO ()
jumpRel r | r < 0 || r >= 1 = pure ()
          | True = modifySTM \st ->
                pure st { cursor = floor $ fromIntegral (size st) * r }

-- | Generic jump
jumpTo :: HState -> (Int -> Int) -> IO HState
jumpTo st fn = do
    let l = max 0 (size st - 1)
        i = fn (cursor st)
        n | i > l = l
          | i < 0 = 0
          | True  = i
    pure st { cursor = n }

------------------------------------------------------------------------

-- | Load and play the song under the cursor
play :: IO ()
play = modifySTM $ \st ->
    if current st == cursor st
    then jumpToRandom st
    else playAtN st (const $ cursor st)

playCur :: IO ()
playCur = modifySTM $ \st -> playAtN st (const $ cursor st)

blacklist :: IO ()
blacklist = do
  st <- getsST id
  appendFile ".hmp3-delete" . (++"\n") . P.unpack $
    let fe = music st ! cursor st
    in P.intercalate (P.singleton '/') [dname $ folders st ! fdir fe, fbase fe]


-- | Play a random song
playRandom :: IO ()
playRandom = modifySTM jumpToRandom

-- | Jump to a random song
jumpToRandom :: HState -> IO HState
jumpToRandom st = do
    n' <- randomIO
    let n = abs n' `mod` (size st - 1)
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
        | otherwise         -> pure    st            -- else stop at end
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
        | otherwise         -> pure    st            -- else stop at end
      }

-- | Generic next song selection
-- If the cursor and current are currently the same, continue that.
playAtN :: HState -> (Int -> Int) -> IO HState
playAtN st fn = do
    let m   = music st
        i   = current st
        fe  = m ! fn i
        -- unsure of this GBH (2008)
        f   = P.intercalate (P.singleton '/')
                 [dname $ folders st ! fdir fe, fbase fe]
        j   = cursor  st
        st' = st { current = fn i
                 , status  = Playing
                 , cursor  = if i == cursor st then fn i else j }
    h <- readMVar (writeh st)
    send h (Load f)
    pure st'

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
jumpToPlaying = modifyST $ \st -> st { cursor = current st }

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
    in st { cursor = dlo (folders st ! d) }

------------------------------------------------------------------------

--
-- a bit of bounded parametric polymorphism so we can abstract over record selectors
-- in the regex search stuff below
--
class Lookup a       where extract :: a -> FilePathP
instance Lookup Tree.Dir  where extract = dname
instance Lookup Tree.File where extract = fbase

jumpToMatchFile :: Maybe String -> Bool -> IO ()
jumpToMatchFile re sw = genericJumpToMatch re sw k sel
    where k = \st -> (music st, if size st == 0 then -1 else cursor st, size st)
          sel i _ = i

jumpToMatch  :: Maybe String -> Bool -> IO ()
jumpToMatch     re sw = genericJumpToMatch re sw k sel
    where k = \st -> (folders st
                     ,if size st == 0 then -1 else fdir (music st ! cursor st)
                     ,1 + (snd . bounds $ folders st))
          sel i st = dlo (folders st ! i)

genericJumpToMatch :: Lookup a
                   => Maybe String
                   -> Bool
                   -> (HState -> (Array Int a, Int, Int))
                   -> (Int -> HState -> Int)
                   -> IO ()

genericJumpToMatch re sw k sel = do
    found <- modifySTM_ $ \st -> do
        let mre = case re of
            -- work out if we have no pattern, a cached pattern, or a new pattern
                Nothing     -> case regex st of
                                Nothing     -> Nothing
                                Just (r,d)  -> Just (r,d==sw)
                Just s  -> case compileM (P.pack s) [caseless] of
                                Left _      -> Nothing
                                Right v     -> Just (v,sw)
        case mre of
            Nothing -> pure (st,False)    -- no pattern
            Just (p,forwards) -> do

            let (fs,cur,m) = k st

{-
                loop fn inc n
                    | fn n      = pure Nothing
                    | otherwise = do
                        let s = extract (fs ! n)
                        case match p s [] of
                            Nothing -> loop fn inc $! inc n
-}

                check n = let s = extract (fs ! n) in
                        case match p s [] of
                            Nothing -> pure Nothing
                            Just _  -> pure $ Just n

            -- mi <- if forwards then loop (>=m) (+1)         (cur+1)
                              -- else loop (<0)  (subtract 1) (cur-1)
            mi <- fmap msum $ mapM check $
                       if forwards then [cur+1..m-1] ++ [0..cur]
                                   else [cur-1,cur-2..0] ++ [m-1,m-2..cur]


            let st' = st { regex = Just (p,forwards==sw) }
            pure case mi of
                Nothing -> (st',False)
                Just i  -> (st' { cursor = sel i st }, True)

    unless found $ putmsg (Fast "No match found." defaultSty) *> touchST

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
    withST \st -> do
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
    if b then Just <$!> readTree f else pure Nothing

-- | Find a user's home in a canonical sort of way
getHome :: IO String
getHome = catch @SomeException
    do getRealUserID >>= getUserEntryForID <&> homeDirectory
    do const $ getEnv "HOME"

------------------------------------------------------------------------
-- Read styles from ~/.hmp3
--
loadConfig :: IO ()
loadConfig = do
    home <- getHome
    let f = home </> ".hmp3"
    b <- doesFileExist f
    if b then do
        str  <- readFile f
        msty <- catch (fmap Just $ evaluate $ read str)
                      (\ (_ :: SomeException) ->
                        warnA "Parse error in ~/.hmp3" $> Nothing)
        case msty of
            Nothing  -> pure ()
            Just rsty -> do
                let sty = buildStyle rsty
                initcolours sty
                modifyST $ \st -> st { config = sty }
      else do
        let sty = config emptySt
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
