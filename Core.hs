{-# LANGUAGE CPP, AllowAmbiguousTypes #-}

-- 
-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2008, 2019-2025 Galen Huntington
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
        forcePause, quit, putmsg, clrmsg, toggleHelp, play, playCur,
        jumpToPlaying, jump, jumpRel,
        upPage, downPage,
        seekStart,
        blacklist,
        showHist, hideHist,
        jumpToMatch, jumpToMatchFile,
        toggleFocus, jumpToNextDir, jumpToPrevDir,
        loadConfig,
        discardErrors,
        toggleExit,
    ) where

import Base

import Syntax
import Lexer                (parser)
import State
import Style
import FastIO               (send, FiltHandle(..), newFiltHandle)
import Tree hiding (File, Dir)
import qualified Tree (File,Dir)
import qualified UI

import Text.Regex.PCRE.Light
import {-# SOURCE #-} Keymap (keymap)

import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Sequence as Seq

import Data.Array               ((!), bounds, Array)
import System.Directory         (doesFileExist, findExecutable, createDirectoryIfMissing,
                                 getXdgDirectory, XdgDirectory(..))
import System.IO                (hPutStrLn, hGetLine, stderr, hFlush)
import System.Process           (runInteractiveProcess, waitForProcess)
import System.Clock             (TimeSpec(..), diffTimeSpec)
import System.Random            (randomIO)
import System.FilePath          ((</>))

import System.Posix.Process     (exitImmediately)


mp3Tool :: String
mp3Tool =
#ifdef MPG321
    "mpg321"
#else
    "mpg123"
#endif

------------------------------------------------------------------------

start :: Bool -> Tree -> IO ()
start playNow (Tree ds fs) = handle @SomeException (shutdown . Just . show) do

    t0 <- forkIO mpgLoop    -- start this off early, to give mpg123 time to settle

    c <- UI.start -- initialise curses

    now <- getMonoTime
    mode <- readState

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
        , cursor       = 0
        , current      = 0
        , mode         = mode
        , uptime       = showTimeDiff now now
        , boottime     = now
        , config       = c
        , threads      = [t0,t1,t2,t3,t4,t5] }

    loadConfig

    when (0 <= (snd . bounds $ fs)) do
        if mode == Random then modifySTM jumpToRandom else playCur
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

-- | Standard combinator.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mb f = maybe (pure ()) f mb

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

-- | Process loop, launch mpg123, set the handles in the state
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
      Just mppath -> do

        -- if we're never able to start mpg123, do something sensible
        mv <- catch (pure <$> runInteractiveProcess mppath ["-R", "-"] Nothing Nothing)
                    (\ (e :: SomeException) ->
                           do warnA ("Unable to start " ++ mp3Tool ++ ": " ++ show e)
                              pure Nothing)

        whenJust mv \ (hw, r, e, pid) -> do

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
                  , id3       = Nothing
                  }

            catch @SomeException (void $ waitForProcess pid) (\_ -> pure ())
            stop <- getsST doNotResuscitate
            when stop exitSuccess
            warnA $ "Restarting " ++ mppath ++ " ..."

        -- Delay to slow spawn loops in case of trouble.
        threadDelay 4_000_000


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
    now <- getMonoTime
    modifyST $ \st -> st { uptime = showTimeDiff (boottime st) now }
  where
    delay = 5 * 1000 * 1000 -- refresh every 5 seconds

------------------------------------------------------------------------

showTimeDiff_ :: Bool -> TimeSpec -> TimeSpec -> ByteString
showTimeDiff_ secs before now
    | ms == 0 && secs
              = go ""
    | hs == 0 = go $ printf "%dm" m
    | d == 0  = go $ printf "%dh%02dm" h m
    | True    = go $ printf "%dd%02dh%02dm" d h m
    where
        go     = P.pack . ss
        stot   = sec $ diffTimeSpec before now
        (ms,s) = quotRem stot 60
        (hs,m) = quotRem ms 60
        (d,h)  = quotRem hs 24
        ss     = if secs then (<> printf (if ms > 0 then "%02ds" else "%ds") s) else id

showTimeDiff :: TimeSpec -> TimeSpec -> ByteString
showTimeDiff = showTimeDiff_ False

------------------------------------------------------------------------

-- | Once each half second, wake up and redraw the clock
clockLoop :: IO ()
clockLoop = runForever $ threadDelay delay >> UI.refreshClock
  where
    delay = 500 * 1000 -- 0.5 second

------------------------------------------------------------------------

-- | Handle, and display errors produced by mpg123
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
        discardErrors writeState
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
--   TODO why is this in IO?
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
playPrev = modifySTM \st -> case mode st of
    Random -> jumpToRandom st
    Single -> pure st
    _ | current st > 0
           -> playAtN st (subtract 1)
    Loop   -> playAtN st (const (size st - 1))
    Once   -> pure st

-- | Play the song following the current song, if we're not at the end
-- If we're at the end, and loop mode is on, then loop to the start
-- If we're in random mode, play the next random track
playNext :: IO ()
playNext = modifySTM \st -> case mode st of
    Random -> jumpToRandom st
    Single -> pure st
    _ | current st < size st - 1
           -> playAtN st (+ 1)
    Loop   -> playAtN st (const 0)
    Once   -> pure st

-- | Generic next song selection
-- If the cursor and current are currently the same, continue that.
playAtN :: HState -> (Int -> Int) -> IO HState
playAtN st fn = do
    now <- getMonoTime
    let m   = music st
        i   = current st
        new = fn i
        fe  = m ! new
        -- unsure of this GBH (2008)
        f   = P.intercalate (P.singleton '/')
                 [dname $ folders st ! fdir fe, fbase fe]
        j   = cursor  st
        st' = st { current = new
                 , status  = Playing
                 , cursor  = if i == cursor st then new else j
                 , playHist = Seq.take 36 $ (now, new) Seq.<| playHist st
                 }
    h <- readMVar (writeh st)
    send h (Load f)
    pure st'

------------------------------------------------------------------------

-- | Toggle pause on the current song
pause :: IO ()
pause = withST $ \st -> readMVar (writeh st) >>= flip send Pause

-- | Always pause
forcePause :: IO ()
forcePause = do
    st <- getsST status
    when (st == Playing) pause

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
    where k st = (music st, if size st == 0 then -1 else cursor st, size st)
          sel i _ = i

jumpToMatch  :: Maybe String -> Bool -> IO ()
jumpToMatch     re sw = genericJumpToMatch re sw k sel
    where k st = (folders st
                     , if size st == 0 then -1 else fdir (music st ! cursor st)
                     , 1 + (snd . bounds $ folders st))
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
                Just s  -> case compileM (P.pack s) [caseless,utf8] of
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
            mi <- fmap msum $ traverse check $
                       if forwards then [cur+1..m-1] ++ [0..cur]
                                   else [cur-1,cur-2..0] ++ [m-1,m-2..cur]


            let st' = st { regex = Just (p,forwards==sw) }
            pure case mi of
                Nothing -> (st',False)
                Just i  -> (st' { cursor = sel i st }, True)

    unless found $ putmsg (Fast "No match found." defaultSty) *> touchST

------------------------------------------------------------------------

-- | Show/hide the help window
toggleHelp :: IO ()
toggleHelp = modifyST $ \st -> st { helpVisible = not (helpVisible st) }

-- | Focus the minibuffer
toggleFocus :: IO ()
toggleFocus = modifyST $ \st -> st { miniFocused = not (miniFocused st) }

-- | Show/hide the confirm exit modal
toggleExit :: IO ()
toggleExit = modifyST $ \st -> st { exitVisible = not (exitVisible st) }

-- | History on or off
hideHist :: IO ()
hideHist = modifyST $ \st -> st { histVisible = Nothing }
showHist :: IO ()
showHist = do
    now <- getMonoTime
    modifyST $ \st -> st {
        helpVisible = False,
        histVisible = Just $ do
            (tm, ix) <- toList $ playHist st
            pure (UTF8.toString $ showTimeDiff_ True tm now
                , (ix, UTF8.toString $ fbase $ music st ! ix))
        }

-- | Toggle the mode flag
nextMode :: IO ()
nextMode = modifyST $ \st -> st { mode = next (mode st) }
    where
        next v = if v == maxBound then minBound else succ v

------------------------------------------------------------------------

getStatePath :: IO FilePath
getStatePath = getXdgDirectory XdgState "hmp3"

-- | Save mode state
writeState :: IO ()
writeState = do
    dir <- getStatePath
    createDirectoryIfMissing True dir
    mode <- getsST mode
    writeFile (dir </> "mode") $ show mode ++ "\n"

-- | Read mode state
readState :: IO Mode
readState = do
    dir <- getStatePath
    let f = dir </> "mode"
    b <- doesFileExist f
    modeM <- if b
        then readMaybe <$!> readFile f
        else pure Nothing
    pure $ fromMaybe (mode emptySt) modeM

------------------------------------------------------------------------
-- Read styles from style.conf
--

getConfPath :: IO FilePath
getConfPath = getXdgDirectory XdgConfig $ "hmp3" </> "style.conf"

loadConfig :: IO ()
loadConfig = do
    f <- getConfPath
    b <- doesFileExist f
    if b then do
        str' <- readFile f
        str <- let (old, new) = ("hmp3_helpscreen", "hmp3_modals") in
            case findIndex (old `isPrefixOf`) $ tails str' of
                Just ix -> do
                    warnA $ old ++ " is now " ++ new ++ " in style.conf"
                    pure $ take ix str' ++ new ++ drop (ix + length old) str'
                _ -> pure str'
        case readMaybe str of
            Nothing  -> do
                warnA "Parse error in style.conf"
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

