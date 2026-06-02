{-# LANGUAGE CPP #-}

-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2008, 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

--
-- | Main module. 
--
module Core (
    Options(..),
    start,
    shutdown,
    seekLeft, seekRight, upOne, downOne, pause, nextMode, playNext, playPrev,
    forcePause, putmsg, clrmsg, play, playCur,
    jumpToPlaying, jump, jumpRel,
    upPage, downPage,
    seekStart,
    blacklist,
    setsModal, closeModal, showHist,
    jumpToMatchDir, jumpToMatchFile,
    toggleFocus, jumpToNextDir, jumpToPrevDir,
    loadConfig,
    discardErrors,
    showTimeDiff_,
) where

import Base

import Syntax
import Lexer                (parser)
import State
import Style
import Tree hiding (File, Dir)
import qualified Tree (File,Dir)
import qualified UI

import Text.Regex.PCRE.Light
import {-# SOURCE #-} Keymap (keyLoop)

import qualified Data.ByteString.Char8 as P
import qualified Data.Sequence as Seq

import Data.Array               ((!), bounds, Array)
import Data.Proxy
import Data.Tuple               (swap)
import Control.Monad.State.Strict
import System.Directory         (doesFileExist, findExecutable, createDirectoryIfMissing,
                                 getXdgDirectory, XdgDirectory(..))
import System.IO                (hPutStrLn, hGetLine, stderr, hFlush)
import System.Process           (runInteractiveProcess, waitForProcess)
import System.Clock             (TimeSpec(..), diffTimeSpec)
import System.Random            (randomR)
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

-- | Command-line configuration.
data Options = Options
    { optPaused     :: !Bool             -- ^ start in a paused state
    , optConfigPath :: !(Maybe FilePath) -- ^ override the style.conf location
    }

start :: Options -> Tree -> IO ()
start opts (Tree ds fs) = handle @SomeException (shutdown . Just . show) do

    c <- UI.start -- initialise curses

    now <- getMonoTime
    mode <- readState

    threads <- traverse forkIO
        [ mpgLoop
        , mpgInput readf
        , refreshLoop
        , clockLoop
        , uptimeLoop
        -- mpg321 uses stderr for @F messages
        , if mp3Tool == "mpg321" then mpgInput errh else errorLoop
        ]

    silentlyModifyHS $ \st -> st
        { music        = fs
        , folders      = ds
        , size         = 1 + (snd . bounds $ fs)
        , cursor       = 0
        , current      = 0
        , mode         = mode
        , boottime     = now
        , config       = c
        , configPath   = optConfigPath opts
        , threads      }

    loadConfig

    if mode == Random then runPlayOp playRandomOp else playCur
    when (optPaused opts) pause

    run         -- won't restart if this fails!

------------------------------------------------------------------------

-- | Uniform loop and thread handler
runForever :: IO () -> IO ()
runForever fn = catch (forever fn) handler where
    handler :: SomeException -> IO ()
    handler e = unless (exitTime e) do
        warnA $ "outer: " ++ show e
        threadDelay 50_000
        runForever fn

-- | Generic handler
-- I don't know why these are ignored, but preserving old logic.
-- For profiling, make sure to return True for anything:
exitTime :: SomeException -> Bool
exitTime e | is @IOException Proxy e = False -- ignore
           | is @ErrorCall Proxy e   = False -- ignore
           -- "user errors" were caught before, but are no longer a thing
           | otherwise               = True
    where is :: forall e. Exception e => Proxy e -> SomeException -> Bool
          is _ = isJust . fromException @e

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
      Nothing     -> shutdown $ Just $ "Cannot find " ++ mp3Tool ++ " in path"
      Just mppath -> do
        mv <- try $ runInteractiveProcess mppath ["-R", "-"] Nothing Nothing
        case mv of
          Left (ex :: SomeException) ->
            warnA $ mppath ++ " failed to start; retrying: " ++ show ex

          Right (writeh, r, e, pid) -> do
            ct <- modifyHS $ \st -> let sp = spawns st + 1 in (st
                { mpgPid    = Just pid
                , status    = Stopped
                , info      = Nothing
                , id3       = Nothing
                , spawns    = sp
                }, sp)

            readf <- newFiltHandle r
            errh <- newFiltHandle e
            putMVar mpg Mpg { readf, errh, writeh }

            when (ct > 1) $ warnA $ mp3Tool ++ " #" ++ show ct ++ ": Ready"
            catch @SomeException (void $ waitForProcess pid) (const $ pure ())

            -- Must be in this order or risk shutdown deadlock!
            silentlyModifyHS $ \st -> st { mpgPid = Nothing }
            void $ takeMVar mpg

            stop <- getsHS doNotResuscitate
            when stop exitSuccess
            threadDelay 1_000_000  -- let threads spit errors
            warnA $ "Restarting " ++ mppath ++ " ..."

        -- Slow spawn loops in case of trouble.
        threadDelay 4_000_000


------------------------------------------------------------------------

-- | When the editor state has been modified, refresh, then wait
-- for it to be modified again.
refreshLoop :: IO ()
refreshLoop = do
    mvar <- getsHS modified
    runForever $ takeMVar mvar >> UI.refresh

------------------------------------------------------------------------

-- | The clock ticks once per minute, but check more often in case of drift.
uptimeLoop :: IO ()
uptimeLoop = runForever $ do
    now <- getMonoTime
    modifyHS_ $ \st -> st { uptime = showTimeDiff (boottime st) now }
    threadDelay 3_000_000

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

-- | Periodically wake up and redraw the clock
clockLoop :: IO ()
clockLoop = runForever $ threadDelay 125_000 *> UI.refreshClock

------------------------------------------------------------------------

-- | Handle, and display errors produced by mpg123
errorLoop :: IO ()
errorLoop = runForever $
    readMVar mpg <&> errh >>= hGetLine . filtHandle >>= (warnA . ("mpg: " ++))

------------------------------------------------------------------------

-- | Handle messages arriving over a pipe from the decoder process. When
-- shutdown kills the other end of the pipe, hGetLine will fail, so we
-- take that chance to exit.
--
mpgInput :: (Mpg -> FiltHandle) -> IO ()
mpgInput field = runForever $ do
    res <- parser =<< field <$> readMVar mpg
    case res of
        Right m       -> handleMsg m
        Left (Just e) -> (warnA . ("read: " ++) . show) e
        _             -> pure ()

------------------------------------------------------------------------

-- | The main thread: handle keystrokes fed to us by curses
run :: IO ()
run = runForever keyLoop

------------------------------------------------------------------------

-- | Close most things. Important to do all the jobs:
shutdown :: Maybe String -> IO ()
shutdown ms = do
    silentlyModifyHS $ \st -> st { doNotResuscitate = True }
    discardErrors writeState
    mpid <- getsHS mpgPid
    whenJust mpid \pid -> do
        discardErrors $ sendMpg Quit
        void $ waitForProcess pid
    UI.end =<< getsHS xterm
    whenJust ms \s -> hPutStrLn stderr s *> hFlush stderr
    exitImmediately ExitSuccess

------------------------------------------------------------------------
-- 
-- Write incoming messages from the encoder to the global state in the
-- right pigeon hole.
--
handleMsg :: Msg -> IO ()
handleMsg (T _)                = pure ()
handleMsg (I i)                = modifyHS_ $ \s -> s { info = Just i }
handleMsg (F (File (Left  _))) = modifyHS_ $ \s -> s { id3 = Nothing }
handleMsg (F (File (Right i))) = modifyHS_ $ \s -> s { id3 = Just i  }

handleMsg (S t) = do
    modifyHS_ $ \s -> s { status  = t }
    when (t == Stopped) playNext   -- transition to next song

handleMsg (R f) = do
    silentlyModifyHS \st -> st { clock = Just f }
    getsHS clockUpdate >>= flip when UI.refreshClock

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
    f <- getsHS clock
    case f of
        Nothing -> pure ()
        Just g  -> do
            sendMpg $ Jump (fn g)
            forceNextPacket         -- don't drop the next Frame.
            silentlyModifyHS $ \st -> st { clockUpdate = True }


------------------------------------------------------------------------

-- | Generic jump
jumpFn :: (Int -> Int) -> IO ()
jumpFn fn = modifyHS_ \st ->
    st { cursor = (fn (cursor st) `min` (size st - 1)) `max` 0 }

-- | Move cursor up or down
upOne, downOne :: IO ()
upOne   = jumpFn (subtract 1)
downOne = jumpFn (+1)

page :: Int -> IO ()
page dir = do
    (sz, _) <- UI.screenSize
    jumpFn (+ dir*(sz-5))

upPage, downPage :: IO ()
upPage   = page (-1)
downPage = page ( 1)

-- | Move cursor to specified index
jump :: Int -> IO ()
jump = jumpFn . const

-- | Jump to relative place, 0 to 1.
jumpRel :: Float -> IO ()
jumpRel r | r < 0 || r >= 1 = pure ()
          | True = modifyHS_ $ \st ->
              st { cursor = floor $ fromIntegral (size st) * r }

-- | Experimental feature concept.
blacklist :: IO ()
blacklist = do
    st <- getsHS id
    appendFile ".hmp3-delete" . (++"\n") . P.unpack $
        let fe = music st ! cursor st
        in P.intercalate (P.singleton '/') [dname $ folders st ! fdir fe, fbase fe]

------------------------------------------------------------------------

-- | Operates on HState and outputs maybe a track to play.
type PlayOp = State HState (Maybe Int)

-- | Play the song under the cursor or random if that one is playing
-- TODO these semantics are strange; maybe playNext instead of random?
play :: IO ()
play = runPlayOp do
    HState { current, cursor } <- get
    if current == cursor
        then playRandomOp
        else pure $ Just cursor

-- | Play the song under the cursor (from the start)
playCur :: IO ()
playCur = runPlayOp $ Just <$> gets cursor

-- | Play the song before the current song, if we're not at the beginning
-- If we're at the beginning, and loop mode is on, then loop to the end
-- If we're in random mode, play the next random track
playPrev :: IO ()
playPrev = runPlayOp do
    HState { mode, size, current } <- get
    case mode of
        Random  -> playRandomOp
        Single  -> pure Nothing
        _ | current > 0
                -> pure $ Just $ current - 1
        Loop    -> pure $ Just $ size - 1
        Once    -> pure Nothing

-- | Play the song following the current song, if we're not at the end
-- If we're at the end, and loop mode is on, then loop to the start
-- If we're in random mode, play the next random track
playNext :: IO ()
playNext = runPlayOp do
    HState { mode, current, size } <- get
    let next = current + 1
    case mode of
        Random  -> playRandomOp
        Single  -> pure Nothing
        _ | next < size
                -> pure $ Just next
        Loop    -> pure $ Just 0
        Once    -> pure Nothing

-- | Random song
playRandomOp :: PlayOp
playRandomOp = do
    HState { size, randomGen } <- get
    let (new, gen') = randomR (0, size-1) randomGen
    modify' \st -> st { randomGen = gen' }
    pure $ Just new

-- | Generic next song selection
-- If cursor is on current, drag it along.
runPlayOp :: PlayOp -> IO ()
runPlayOp op = do
    now <- getMonoTime
    mfile <- modifyHS $ swap . runState do
        mnew <- op
        forM mnew \new -> do
            HState { .. } <- get
            let fe = music ! new
                f  = P.intercalate (P.singleton '/')
                        [dname $ folders ! fdir fe, fbase fe]
            modify' \st -> st
                { current = new
                , status  = Playing
                , cursor  = if current == cursor then new else cursor
                , playHist = Seq.take 36 $ (now, new) Seq.<| playHist
                }
            pure f
    forM_ mfile $ sendMpg . Load

------------------------------------------------------------------------

-- | Toggle pause on the current song
pause :: IO ()
pause = sendMpg Pause

-- | Always pause
forcePause :: IO ()
forcePause = do
    st <- getsHS status
    when (st == Playing) pause

------------------------------------------------------------------------

-- | Move cursor to currently playing song
jumpToPlaying :: IO ()
jumpToPlaying = modifyHS_ $ \st -> st { cursor = current st }

-- | Move cursor to first song in next directory (or wrap)
jumpToNextDir, jumpToPrevDir :: IO ()
jumpToNextDir = jumpToDir (\i len -> min (i+1) (len-1))
jumpToPrevDir = jumpToDir (\i _   -> max (i-1) 0)

-- | Generic jump to dir
jumpToDir :: (Int -> Int -> Int) -> IO ()
jumpToDir fn = modifyHS_ $ \st -> if size st == 0 then st else
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

jumpToMatchDir :: Maybe String -> Bool -> IO ()
jumpToMatchDir re sw = genericJumpToMatch re sw k sel
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
    found <- modifyHS $ \st -> do
        let mre = case re of
                Nothing -> case regex st of
                    Nothing     -> Nothing
                    Just (r, d) -> Just (r, d==sw)
                Just s  -> case compileM (P.pack s) [caseless, utf8] of
                    Left _      -> Nothing
                    Right v     -> Just (v, sw)
        flip (maybe (st, False)) mre \ (p, forwards) -> do
            let (fs, cur, m) = k st
                l = if forwards then [cur+1..m-1] ++ [0..cur]
                                else [cur-1,cur-2..0] ++ [m-1,m-2..cur]
                st' = st { regex = Just (p, forwards==sw) }
            case [ i | i <- l, isJust $ match p (extract (fs ! i)) [] ] of
                i:_ -> (st' { cursor = sel i st }, True)
                _   -> (st', False)

    unless found $ putmsg (Fast "No match found." defaultSty) *> touchHS

------------------------------------------------------------------------

-- | General modal setting.
setsModal :: (HState -> Maybe Modal) -> IO ()
setsModal f = modifyHS_ $ \st -> st { modal = f st }

-- | Close any open modal.
closeModal :: IO ()
closeModal = setsModal $ const Nothing

-- | Show history.
showHist :: IO ()
showHist = do
    now <- getMonoTime
    setsModal \st -> Just $ HistModal [
        (showTimeDiff_ True tm now, (ix, fbase $ music st ! ix))
            | (tm, ix) <- toList $ playHist st ]

-- | Focus the minibuffer
toggleFocus :: IO ()
toggleFocus = modifyHS_ $ \st -> st { miniFocused = not (miniFocused st) }

-- | Toggle the mode flag
nextMode :: IO ()
nextMode = modifyHS_ $ \st -> st { mode = next (mode st) } where
    next v = if v == maxBound then minBound else succ v

------------------------------------------------------------------------

getStatePath :: IO FilePath
getStatePath = getXdgDirectory XdgState "hmp3"

-- | Save mode state
writeState :: IO ()
writeState = do
    dir <- getStatePath
    createDirectoryIfMissing True dir
    mode <- getsHS mode
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
    pure $ fromMaybe minBound modeM

------------------------------------------------------------------------
-- Read styles from style.conf
--

getConfPath :: IO FilePath
getConfPath = getXdgDirectory XdgConfig $ "hmp3" </> "style.conf"

loadConfig :: IO ()
loadConfig = do
    f <- maybe getConfPath pure =<< getsHS configPath
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
                modifyHS_ $ \st -> st { config = sty }
    else
        pure () -- TODO in some cases show a warning
    UI.resetui

------------------------------------------------------------------------
-- Editing the minibuffer

-- TODO maybe shouldn't be silent?
putmsg :: StringA -> IO ()
putmsg s = silentlyModifyHS $ \st -> st { minibuffer = s }

-- | Modify without triggering a refresh
clrmsg :: IO ()
clrmsg = putmsg (Fast P.empty defaultSty)

--
warnA :: String -> IO ()
warnA x = do
    sty <- getsHS config
    putmsg $ Fast (P.pack x) (warnings sty)
    touchHS

