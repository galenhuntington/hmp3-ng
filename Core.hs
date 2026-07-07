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
    jumpToPlaying, jump, jumpRel, jumpRandom,
    upPage, downPage,
    seekStart,
    blacklist,
    setsModal, closeModal, showHist,
    jumpToMatchDir, jumpToMatchFile,
    toggleFocus, jumpToNextDir, jumpToPrevDir,
    loadConfig,
    discardErrors,
) where

import Base

import Decoder
import State
import Style
import Playlist
import Text (matches)
import UI qualified
import Elements qualified as El

import Data.ByteString.Char8 qualified as P
import Data.Sequence qualified as Seq

import Data.Array               ((!), Array)
import Data.Proxy
import Data.Tuple               (swap)
import Control.Monad.Except
import Control.Monad.State.Strict
import System.Directory         (doesFileExist, findExecutable, createDirectoryIfMissing,
                                 getXdgDirectory, XdgDirectory(..))
import System.IO                (hPutStrLn, stderr)
import System.Process           (runInteractiveProcess, waitForProcess)
import System.Random            (randomR, newStdGen)
import System.FilePath qualified as FP ((</>))
import System.Posix.FilePath    (takeFileName, (</>))
import System.Posix.Process     (exitImmediately)


------------------------------------------------------------------------

-- | Command-line configuration.
data Options = Options
    { paused     :: !Bool             -- ^ start in a paused state
    , configPath :: !(Maybe FilePath) -- ^ override the style.conf location
    , playMode   :: Maybe Mode        -- ^ play mode
    , histSize   :: Int               -- ^ history size
    , random     :: Bool              -- ^ start on random song
    }

-- | Sets up state, spawns sub-threads, and starts player.
start :: Options -> Playlist -> IO ()
start opts (Playlist folders music) = do

    uiStyle <- UI.start
    bootTime <- getMonoTime
    mode <- maybe readState pure opts.playMode
    gen <- newStdGen
    let (current, randomGen) = if mode == Random || opts.random
        then randomR (0, length music - 1) gen else (0, gen)

    putMVar hState HState
        { music
        , folders
        , bootTime
        , configPath   = opts.configPath
        , current
        , cursor       = current
        , randomGen
        , mode
        , uiStyle
        , spawns       = 0
        , clock        = Nothing
        , info         = Nothing
        , id3          = Nothing
        , modal        = Nothing
        , playHist     = mempty
        , searchHist   = []
        , searchFw     = True
        , histSize     = opts.histSize
        , miniFocused  = False
        , status       = Stopped
        , minibuffer   = []
        , uptime       = mempty
        }

    loadConfig  -- TODO this should return config rather than setting it

    traverse_ forkIO
        [ mpgLoop
        , mpgInput
        , refreshLoop
        , uptimeLoop
        ]

    -- Poll for a second for process to start, then play.
    let go :: Int -> IO ()
        go 0 = silentlyModifyHS \st -> st { spawns = 1 }
        go n = do
            ready <- isJust <$> readIORef mpgRef
            if ready
                then do
                    playCur
                    when opts.paused pause -- TODO use LOADPAUSED?
                else do
                    threadDelay 20_000
                    go (n-1)
    go 50


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

-- | Loop, launching decoder and updating global state.
mpgLoop :: IO ()
mpgLoop = runForever do
    empg <- runExceptT do
        mppath <- lift (findExecutable mp3Tool) >>=
            maybe (throwError $ "Cannot find " ++ mp3Tool ++ " in path") pure
        lift (try @SomeException $
            runInteractiveProcess mppath ["-R", "--remote-err"] Nothing Nothing
            ) >>= flip either pure \ex ->
                throwError $ mp3Tool ++ " failed to start; retrying: " ++ show ex
    case empg of
        Left err -> do
            warnA err
            -- Hackily count failed initial spawn, for Ready message
            silentlyModifyHS \st -> st { spawns = st.spawns `max` 1 }
            threadDelay 20_000_000  -- longer wait after these errors
        Right handles -> do
            ct <- modifyHS $ \st -> let sp = st.spawns + 1 in (st
                { status    = Stopped
                , info      = Nothing
                , id3       = Nothing
                , spawns    = sp
                }, sp)
            when (ct > 1) $ warnA $ mp3Tool ++ " #" ++ show ct ++ ": Ready"
            overseeMpg handles
            threadDelay 1_000_000  -- let threads spit errors
            warnA $ "Restarting " ++ mp3Tool ++ " ..."
    threadDelay 4_000_000  -- rate-limit respawns

------------------------------------------------------------------------

-- | When the editor state has been modified, refresh, then wait
-- for it to be modified again.
refreshLoop :: IO ()
refreshLoop = runForever $ takeMVar modified *> UI.refresh

------------------------------------------------------------------------

-- | The clock ticks once per minute, but check more often in case of drift.
uptimeLoop :: IO ()
uptimeLoop = runForever do
    now <- getMonoTime
    μs <- modifyHS \st -> let diff = now - st.bootTime in
        (st { uptime = El.showDuration False diff }, diff `div` 1000)
    threadDelay $ fromIntegral $ let m = 60_000_000 in m - μs `mod` m

------------------------------------------------------------------------

-- | Handle messages arriving over a pipe from the decoder process. When
-- shutdown kills the other end of the pipe, hGetLine will fail.
mpgInput :: IO ()
mpgInput = runForever $ do
    line <- P.hGetLine =<< readMVar mpgRead
    case mpgParser line of
        Right m       -> handleMsg m
        Left (Just e) -> warnA (mp3Tool ++ ": " ++ e)
        _             -> pure ()

------------------------------------------------------------------------

-- | Close most things. Important to do all the jobs:
shutdown :: Maybe String -> IO ()
shutdown ms = do
    UI.end
    mpg <- readIORef mpgRef
    whenJust mpg \Mpg { mpgPH } -> do
        discardErrors writeState
        void $ sendMpg' Quit
        void $ waitForProcess mpgPH
    exitImmediately =<< case ms of
        Just s -> hPutStrLn stderr s *> pure (ExitFailure 1)
        _      -> pure ExitSuccess

------------------------------------------------------------------------
-- Process incoming messages from the decoder.

handleMsg :: Msg -> IO ()
handleMsg (S i)   = modifyHS_ $ \st -> st { info = Just i }
handleMsg (I id3) = modifyHS_ $ \st -> st { id3 = Just id3 }
handleMsg (P t)   = do
    modifyHS_ \st -> st
        { status = t
        , clock = case st.clock of
            Just f@Frame{ timeLeft } | t == Stopped && timeLeft < 0.1
                -> Just f { timeLeft = 0 } -- force clock to end if near
            c   -> c
        }
    when (t == Stopped) playNext
handleMsg (F f) = do
    silentlyModifyHS \st -> st { clock = Just f }
    UI.refreshClock

------------------------------------------------------------------------
-- Basic operations

-- | Seek backward in song
seekLeft :: IO ()
seekLeft = seek \g -> max 0 (g.currentFrame - 400)

-- | Seek forward in song
seekRight :: IO ()
seekRight = seek \g -> g.currentFrame + min 400 g.framesLeft

seekStart :: IO ()
seekStart = seek $ const 0

-- | Generic seek
seek :: (Frame -> Int) -> IO ()
seek fn = do
    mfr <- getsHS (.clock)
    whenJust mfr \fr -> sendMpg $ Jump $ fn fr

------------------------------------------------------------------------

-- | Generic jump
jumpFn :: (Int -> Int) -> IO ()
jumpFn fn = modifyHS_ \st ->
    st { cursor = (fn st.cursor `min` (st.size - 1)) `max` 0 }

-- | Move cursor up or down
upOne, downOne :: IO ()
upOne   = jumpFn (subtract 1)
downOne = jumpFn (+ 1)

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
              st { cursor = floor $ fromIntegral st.size * r }

-- | Experimental feature concept.
blacklist :: IO ()
blacklist = do
    st <- getsHS id
    appendFile ".hmp3-delete" . (++"\n") . P.unpack $
        let fe = st.music ! st.cursor
        in (st.folders ! fe.fdir).dname </> fe.fbase

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
playCur = runPlayOp $ Just <$> gets (.cursor)

-- | Play the song before the current song, if we're not at the beginning
-- If we're at the beginning, and loop mode is on, then loop to the end
-- If we're in random mode, play the next random track
playPrev :: IO ()
playPrev = runPlayOp do
    st <- get
    case st.mode of
        Random  -> playRandomOp
        Single  -> pure Nothing
        _ | st.current > 0
                -> pure $ Just $ st.current - 1
        Loop    -> pure $ Just $ st.size - 1
        Once    -> pure Nothing

-- | Play the song following the current song, if we're not at the end
-- If we're at the end, and loop mode is on, then loop to the start
-- If we're in random mode, play the next random track
playNext :: IO ()
playNext = runPlayOp playNextOp

playNextOp :: PlayOp
playNextOp = do
    st <- get
    let next = st.current + 1
    case st.mode of
        Random  -> playRandomOp
        Single  -> pure Nothing
        _ | next < st.size
                -> pure $ Just next
        Loop    -> pure $ Just 0
        Once    -> pure Nothing

-- | Generate a random song
getRandom :: State HState Int
getRandom = do
    st <- get
    let (new, gen') = randomR (0, st.size - 1) st.randomGen
    put $ st { randomGen = gen' }
    pure new

-- | Random song
playRandomOp :: PlayOp
playRandomOp = Just <$> getRandom

-- | Jump to random song
jumpRandom :: Bool -> IO ()
jumpRandom play = runPlayOp do
    cursor <- getRandom
    modify' \st -> st { cursor }
    pure $ if play then Just cursor else Nothing

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
                f  = (folders ! fe.fdir).dname </> fe.fbase
            modify' \st -> st
                { current  = new
                , status   = Playing
                , cursor   = if current == cursor then new else cursor
                , playHist = Seq.take histSize $ (now, new) <| playHist
                , id3      = Nothing
                , clock    = Nothing
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
    st <- getsHS (.status)
    when (st == Playing) pause

------------------------------------------------------------------------

-- | Move cursor to currently playing song
jumpToPlaying :: IO ()
jumpToPlaying = modifyHS_ $ \st -> st { cursor = st.current }

-- | Move cursor to first song in next directory (or wrap)
jumpToNextDir, jumpToPrevDir :: IO ()
jumpToNextDir = jumpToDir (\i len -> min (i+1) (len-1))
jumpToPrevDir = jumpToDir (\i _   -> max (i-1) 0)

-- | Generic jump to dir
jumpToDir :: (Int -> Int -> Int) -> IO ()
jumpToDir fn = modifyHS_ \st ->
    let i   = (st.music ! st.cursor).fdir
        d   = fn i (length st.folders)
    in st { cursor = (st.folders ! d).dlo }

------------------------------------------------------------------------

-- a bit of bounded parametric polymorphism so we can abstract over record selectors
-- in the regex search stuff below
class Lookup a       where extract :: a -> RawFilePath
instance Lookup Dir  where extract = takeFileName . (.dname)
instance Lookup File where extract = (.fbase)

jumpToMatchFile :: Maybe ByteString -> Bool -> IO ()
jumpToMatchFile re sw = genericJumpToMatch re sw k sel
    where k st = (st.music, st.cursor, st.size)
          sel i _ = i

jumpToMatchDir :: Maybe ByteString -> Bool -> IO ()
jumpToMatchDir re sw = genericJumpToMatch re sw k sel
    where k st = (st.folders, (st.music ! st.cursor).fdir, length st.folders)
          sel i st = (st.folders ! i).dlo

genericJumpToMatch :: Lookup a
                   => Maybe ByteString
                   -> Bool
                   -> (HState -> (Array Int a, Int, Int))
                   -> (Int -> HState -> Int)
                   -> IO ()
genericJumpToMatch re sw k sel = do
    found <- modifyHS \st -> let
        info = case re of
            Just s -> Just (st { searchFw = sw }, s, sw)
            _      -> listToMaybe [ (st, s, st.searchFw == sw) | s <- st.searchHist ]
        in flip (maybe (st, False)) info \(st', p, forwards) -> do
            let (fs, cur, m) = k st
                l = if forwards then [cur+1 .. m-1] ++ [0 .. cur]
                                else [cur-1, cur-2 .. 0] ++ [m-1, m-2 .. cur]
                match = matches p
            case [ i | i <- l, match $ extract (fs ! i) ] of
                i:_ -> (st' { cursor = sel i st }, True)
                _   -> (st', False)
    unless found $ putMessage [plainSeg "No match found."]

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
        (El.showDuration True (now - tm), (ix, (st.music ! ix).fbase))
            | (tm, ix) <- toList st.playHist ]

-- | Focus the minibuffer
toggleFocus :: IO ()
toggleFocus = modifyHS_ $ \st -> st { miniFocused = not st.miniFocused }

-- | Toggle the mode flag
nextMode :: IO ()
nextMode = modifyHS_ $ \st -> st { mode = next st.mode } where
    next v = if v == maxBound then minBound else succ v

------------------------------------------------------------------------

getStatePath :: IO FilePath
getStatePath = getXdgDirectory XdgState "hmp3"

-- | Save mode state
writeState :: IO ()
writeState = do
    dir <- getStatePath
    createDirectoryIfMissing True dir
    mode <- getsHS (.mode)
    writeFile (dir FP.</> "mode") $ show mode ++ "\n"

-- | Read mode state
readState :: IO Mode
readState = do
    dir <- getStatePath
    let f = dir FP.</> "mode"
    b <- doesFileExist f
    modeM <- if b
        then readMaybe <$!> readFile f
        else pure Nothing
    pure $ fromMaybe minBound modeM

------------------------------------------------------------------------
-- Read styles from style.conf
--

getConfPath :: IO FilePath
getConfPath = getXdgDirectory XdgConfig $ "hmp3" FP.</> "style.conf"

loadConfig :: IO ()
loadConfig = do
    f <- maybe getConfPath pure =<< getsHS (.configPath)
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
                modifyHS_ $ \st -> st { uiStyle = sty }
    else
        pure () -- TODO in some cases show a warning
    UI.resetui

------------------------------------------------------------------------
-- Set the minibuffer

putMessage :: Line -> IO ()
putMessage s = modifyHS_ \st -> st { minibuffer = s }

clearMessage :: IO ()
clearMessage = putMessage []

warnA :: String -> IO ()
warnA x = do
    sty <- getsHS (.uiStyle.warnings)
    putMessage [Seg sty (P.pack x)]

