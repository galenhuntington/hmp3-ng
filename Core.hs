-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2008, 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

--
-- | Main module. 
--
module Core (
    Options(..),
    start, shutdown,
    seekLeft, seekRight, upOne, downOne, pause, nextMode, playNext, playPrev,
    forcePause, putMessage, clearMessage, playCursor, playCur,
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

import Decoder
import State
import Style
import Playlist
import UI qualified

import Data.ByteString.Char8 qualified as P
import Data.Sequence qualified as Seq

import Data.Array               ((!), Array)
import Data.Proxy
import Data.Tuple               (swap)
import Control.Monad.State.Strict
import System.Directory         (doesFileExist, findExecutable, createDirectoryIfMissing,
                                 getXdgDirectory, XdgDirectory(..))
import System.IO                (hPutStrLn, stderr)
import System.Process           (runInteractiveProcess, waitForProcess)
import System.Clock             (TimeSpec(..), diffTimeSpec)
import System.Random            (randomR, newStdGen)
import System.FilePath          ((</>))
import System.Posix.FilePath    (takeFileName)

import System.Posix.Process     (exitImmediately)


mp3Tool :: String
mp3Tool = "mpg123"

------------------------------------------------------------------------

-- | Command-line configuration.
data Options = Options
    { optPaused     :: !Bool             -- ^ start in a paused state
    , optConfigPath :: !(Maybe FilePath) -- ^ override the style.conf location
    , optPlayMode   :: Maybe Mode        -- ^ play mode
    , optHistSize   :: Int               -- ^ history size
    }

-- | Sets up state, spawns sub-threads, and starts player.
start :: Options -> Playlist -> IO ()
start opts (Playlist folders music) = do

    config <- catch @SomeException UI.start \err -> do
        -- An uncaught exception here would deadlock.
        -- XXX more state model revisions should obviate need
        hPutStrLn stderr $ "Curses failed to start: " ++ show err
        exitImmediately $ ExitFailure 1
    bootTime <- getMonoTime
    let size = length music
    mode <- maybe readState pure (optPlayMode opts)
    gen <- newStdGen
    let (current, randomGen) =
            if mode == Random then randomR (0, size-1) gen else (0, gen)

    threads <- traverse forkIO
        [ mpgLoop
        , mpgInput
        , refreshLoop
        , uptimeLoop
        ]

    putMVar hState HState
        { music
        , folders
        , size
        , bootTime
        , configPath   = optConfigPath opts
        , current
        , cursor       = current
        , randomGen
        , mode
        , config
        , threads
        , spawns       = 0
        , mpgPid       = Nothing
        , clock        = Nothing
        , info         = Nothing
        , id3          = Nothing
        , modal        = Nothing
        , playHist     = mempty
        , searchHist   = []
        , searchFw     = True
        , histSize     = optHistSize opts
        , miniFocused  = False
        , exiting      = False
        , status       = Stopped
        , minibuffer   = Fast mempty defaultSty
        , uptime       = mempty
        }

    loadConfig  -- TODO this should return config rather than setting it

    playCur
    when (optPaused opts) pause -- TODO use LOADPAUSED?

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
        mv <- try $ runInteractiveProcess mppath ["-R", "--remote-err"] Nothing Nothing
        case mv of
          Left (ex :: SomeException) ->
            warnA $ mppath ++ " failed to start; retrying: " ++ show ex

          Right (writeh, _, errh, pid) -> do
            ct <- modifyHS $ \st -> let sp = spawns st + 1 in (st
                { mpgPid    = Just pid
                , status    = Stopped
                , info      = Nothing
                , id3       = Nothing
                , spawns    = sp
                }, sp)

            putMVar mpg Mpg { errh, writeh }

            when (ct > 1) $ warnA $ mp3Tool ++ " #" ++ show ct ++ ": Ready"
            catch @SomeException (void $ waitForProcess pid) (const $ pure ())

            -- Must be in this order or risk shutdown deadlock!
            silentlyModifyHS $ \st -> st { mpgPid = Nothing }
            void $ takeMVar mpg

            stop <- getsHS exiting
            when stop exitSuccess
            threadDelay 1_000_000  -- let threads spit errors
            warnA $ "Restarting " ++ mppath ++ " ..."

        -- Slow spawn loops in case of trouble.
        threadDelay 4_000_000


------------------------------------------------------------------------

-- | When the editor state has been modified, refresh, then wait
-- for it to be modified again.
refreshLoop :: IO ()
refreshLoop = runForever $ takeMVar modified *> UI.refresh


------------------------------------------------------------------------

-- | The clock ticks once per minute, but check more often in case of drift.
uptimeLoop :: IO ()
uptimeLoop = runForever $ do
    now <- getMonoTime
    modifyHS_ $ \st -> st { uptime = showTimeDiff (bootTime st) now }
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

-- | Handle messages arriving over a pipe from the decoder process. When
-- shutdown kills the other end of the pipe, hGetLine will fail, so we
-- take that chance to exit.
--
mpgInput :: IO ()
mpgInput = runForever $ do
    line <- P.hGetLine =<< errh <$> readMVar mpg
    case mpgParser line of
        Right m       -> handleMsg m
        Left (Just e) -> warnA ("mpg123: " ++ e)
        _             -> pure ()

------------------------------------------------------------------------

-- | Close most things. Important to do all the jobs:
-- TODO maybe releaseSignals here?
shutdown :: Maybe String -> IO ()
shutdown ms = do
    UI.end
    silentlyModifyHS $ \st -> st { exiting = True }
    discardErrors writeState
    mpid <- getsHS mpgPid
    whenJust mpid \pid -> do
        discardErrors $ sendMpg Quit
        void $ waitForProcess pid
    exitImmediately =<< case ms of
        Just s -> hPutStrLn stderr s *> pure (ExitFailure 1)
        _      -> pure ExitSuccess

------------------------------------------------------------------------
-- 
-- Write incoming messages from the encoder to the global state in the
-- right pigeon hole.
--
handleMsg :: Msg -> IO ()

handleMsg (S i)   = modifyHS_ $ \s -> s { info = Just i }

handleMsg (I id3) = modifyHS_ $ \s -> s { id3 = Just id3 }

handleMsg (P t) = do
    modifyHS_ $ \s -> s { status = t }
    when (t == Stopped) playNext   -- transition to next song

handleMsg (F f) = do
    silentlyModifyHS \st -> st { clock = Just f }
    UI.refreshClock

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
    mfr <- getsHS clock
    whenJust mfr \fr -> sendMpg $ Jump $ fn fr

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
jumpRel :: Rational -> IO ()
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

-- | Play the song under the cursor or next if that one is current
playCursor :: IO ()
playCursor = runPlayOp do
    HState { current, cursor } <- get
    if current == cursor then playNextOp else pure $ Just cursor

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
playNext = runPlayOp playNextOp

playNextOp :: PlayOp
playNextOp = do
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
                , playHist = Seq.take histSize $ (now, new) <| playHist
                , id3     = Nothing
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
jumpToDir fn = modifyHS_ \st ->
    let i   = fdir (music st ! cursor st)
        d   = fn i (length $ folders st)
    in st { cursor = dlo (folders st ! d) }

------------------------------------------------------------------------

-- a bit of bounded parametric polymorphism so we can abstract over record selectors
-- in the regex search stuff below
class Lookup a       where extract :: a -> RawFilePath
instance Lookup Dir  where extract = takeFileName . dname
instance Lookup File where extract = fbase

jumpToMatchFile :: Maybe String -> Bool -> IO ()
jumpToMatchFile re sw = genericJumpToMatch re sw k sel
    where k st = (music st, cursor st, size st)
          sel i _ = i

jumpToMatchDir :: Maybe String -> Bool -> IO ()
jumpToMatchDir re sw = genericJumpToMatch re sw k sel
    where k st = (folders st, fdir (music st ! cursor st), length $ folders st)
          sel i st = dlo (folders st ! i)

genericJumpToMatch :: Lookup a
                   => Maybe String
                   -> Bool
                   -> (HState -> (Array Int a, Int, Int))
                   -> (Int -> HState -> Int)
                   -> IO ()
genericJumpToMatch re sw k sel = do
    found <- modifyHS \st -> let
        info = case re of
            Just s -> Just (st { searchFw = sw }, s, sw)
            _      -> listToMaybe [ (st, s, searchFw st == sw) | s <- searchHist st ]
        in flip (maybe (st, False)) info \(st', p, forwards) -> do
            let (fs, cur, m) = k st
                l = if forwards then [cur+1 .. m-1] ++ [0 .. cur]
                                else [cur-1, cur-2 .. 0] ++ [m-1, m-2 .. cur]
                match = matches (P.pack p)
            case [ i | i <- l, match $ extract (fs ! i) ] of
                i:_ -> (st' { cursor = sel i st }, True)
                _   -> (st', False)
    unless found $ putMessage $ Fast "No match found." defaultSty

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
-- Set the minibuffer

putMessage :: StringA -> IO ()
putMessage s = modifyHS_ \st -> st { minibuffer = s }

clearMessage :: IO ()
clearMessage = putMessage $ Fast P.empty defaultSty

warnA :: String -> IO ()
warnA x = do
    sty <- getsHS config
    putMessage $ Fast (P.pack x) (warnings sty)

