-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- Derived from: riot/UI.hs Copyright (c) Tuomo Valkonen 2004.
-- Released under the same license.

--
-- | This module defines a user interface implemented using ncurses. 
--
--

module UI (
    runDraw,
    -- * Construction, destruction
    start, end, screenSize, refresh, refreshClock, resetui,
    -- * Input
    getKey
  ) where

import Base

import Style
import Playlist                 (File(fdir, fbase), Dir(dname))
import State
import Syntax
import Config
import Width                    (displayWidth, toMaxWidth, toWidth)
import qualified UI.HSCurses.Curses as Curses
import Keyboard                 (unkey, charToKey)

import Data.Array               ((!), bounds, Array)
import Data.Array.Base          (unsafeAt)
import System.Posix.FilePath    (takeFileName)
import System.IO                (stderr, hFlush)
import System.Posix.Signals     (installHandler, Handler(..))

import Foreign.C.String
import Foreign.C.Types
import Foreign.C.Error (Errno(..), getErrno)

import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Unsafe as P
import qualified Data.ByteString.UTF8 as UTF8


-- Write u-strings like it's Python 2.
u :: String -> ByteString
u = UTF8.fromString


newtype Draw = Draw (IO ())
    deriving newtype (Semigroup, Monoid)

drawLock :: MVar ()
drawLock = unsafePerformIO $ newMVar ()
{-# NOINLINE drawLock #-}

runDraw :: Draw -> IO ()
runDraw (Draw io) = withMVar drawLock $ const io

------------------------------------------------------------------------

-- | Initialize the UI
start :: IO UIStyle
start = do
    Curses.initCurses

    case Curses.cursesSigWinch of
        Just wch -> void $ installHandler wch (Catch resetui) Nothing
        _        -> pure () -- handled elsewhere

    colorify <- Curses.hasColors
    let sty = if colorify then defaultStyle else bwStyle

    initcolours sty
    Curses.keypad Curses.stdScr True    -- grab the keyboard
    runDraw nocursor

    pure sty

-- | Reset
resetui :: IO ()
resetui = runDraw (resizeui <> nocursor) *> refresh

-- | And force invisible
nocursor :: Draw
nocursor = Draw $ discardErrors $ void $ Curses.cursSet Curses.CursorInvisible

-- | Clean up and go home.
end :: IO ()
end = do
    takeMVar drawLock        -- we keep so no one tries to draw
    setXtermTitle ["xterm"]  -- XXX I don't see this title after exit?
    Curses.endWin

-- | Find the current screen height and width.
screenSize :: IO (Int, Int)
screenSize = Curses.scrSize

--
-- | Rewrite of Curses.getCh to avoid looping on terminal crash
-- | (also no unget support since I don't need it)
--
getCh :: IO Curses.Key
getCh = do
  threadWaitRead 0
  v <- Curses.getch
  case v of
    -1 -> do
        Errno e <- getErrno
        putStrLn $ "Error " ++ show e ++ "; terminal has gone away?  Hard-exiting now."
        exitFailure
    k -> pure $ Curses.decodeKey k

--
-- | Read a key. UIs need to define a method for getting events.
-- We only need to refresh if we don't have no SIGWINCH support.
--
getKey :: IO Char
getKey = do
    k <- getCh
    if k == Curses.KeyResize 
        then do
              when (isNothing Curses.cursesSigWinch) do
                  runDraw $ redraw <> resizeui
              getKey
        else pure $ unkey k

-- | Resize the window
-- From "Writing Programs with NCURSES", by Eric S. Raymond and Zeyd M. Ben-Halim
--
resizeui :: Draw
resizeui = Draw $ void $ do
    Curses.endWin
    Curses.resetParams
    do
        -- not sure I need all these...
        Curses.nl True
        _ <- Curses.leaveOk True
        Curses.noDelay Curses.stdScr False
        Curses.cBreak True
        -- Curses.meta stdScr True -- not in module
        -- not sure about intrFlush, raw - set in hscurses
    Curses.refresh
    Curses.scrSize

refresh :: IO ()
refresh = runDraw $ redraw <> Draw Curses.refresh

refreshClock :: IO ()
refreshClock = runDraw $ redrawJustClock <> Draw Curses.refresh

------------------------------------------------------------------------

-- (prefix some with underscore to avoid unused warnings)
data Pos  = Pos  { posY, _posX :: !Int }
data Size = Size { _sizeH, sizeW :: !Int }

-- | Renderable widgets are functions @DrawData -> ...@.
data DrawData = DD {
    drawSize  :: Size,
    drawPos   :: Pos,
    drawState :: HState,
    drawFrame :: Maybe Frame
    }

-- screen width -> (modal width, list of lines)
type ModalMaker = Int -> (Int, [ByteString])

------------------------------------------------------------------------

-- | The three lines of the play-mode widget.
playScreen :: DrawData -> [StringA]
playScreen dd = [pPlaying dd, progressBar dd, pTimes dd]

------------------------------------------------------------------------

-- | Info about the current track
pPlaying :: DrawData -> StringA
pPlaying dd = FancyS $ map (, defaultSty) $ "  " : line where
    x = sizeW $ drawSize dd
    a = pId3 dd
    b = pInfo dd
    line | gap >= 0 = a : spaces gap : right
         | True     = toMaxWidth lim a : right
        where lim = x - 5 - (if showId3 then P.length b else -1)
              gap = lim - displayWidth a
              showId3 = x > 59
              right = if showId3 then [" ", b] else []

-- | Id3 info
pId3 :: DrawData -> ByteString
pId3 DD{drawState=st} = case id3 st of
    Just i  -> id3str i
    Nothing -> case size st of
        0 -> "(empty)"
        _ -> fbase $ music st ! current st

-- | mp3 information
pInfo :: DrawData -> ByteString
pInfo DD{drawState=st} = case info st of
    Nothing -> "(empty)"
    Just i  -> userinfo i

commonModalWidth :: Int -> Int
commonModalWidth w = max (min w 3) $ round $ fromIntegral w * (0.8::Float)

------------------------------------------------------------------------

helpModal :: [KeysHelp] -> ModalMaker
helpModal help swd = (wd, map showLine help) where
    wd = commonModalWidth swd
    showLine :: ([Char], ByteString) -> ByteString
    showLine (cs, ps) = toWidth clen cmds <> ps where
        clen = max 4 $ round $ fromIntegral wd * (0.2::Float)
        cmds = P.unwords ("" : map pprIt cs)
        pprIt c = case c of
            '\n' -> "Enter"
            '\f' -> "^L"
            '\\' -> "\\"
            ' '  -> "Space"
            _ -> case charToKey c of
                Curses.KeyUp        -> u"↑"
                Curses.KeyDown      -> u"↓"
                Curses.KeyPPage     -> "PgUp"
                Curses.KeyNPage     -> "PgDn"
                Curses.KeyLeft      -> u"←"
                Curses.KeyRight     -> u"→"
                Curses.KeyEnd       -> "End"
                Curses.KeyHome      -> "Home"
                Curses.KeyBackspace -> "Backspace"
                _ -> u[c]

------------------------------------------------------------------------

histModal :: HistDisplay -> ModalMaker
histModal hist swd = do
    let wd = commonModalWidth swd
        mtlen = maximum $ 0 : map (displayWidth . fst) hist
        tlen = min (mtlen + 1) $ wd `div` 3
    (wd,) $ flip map (zip (['0'..'9']++['a'..'z']) hist) \ (c, (time, (_, song))) ->
        let tstr = toMaxWidth tlen $ P.replicate (tlen - displayWidth time) ' ' <> time
        in mconcat [" ", P.singleton c, " ", tstr, " ", song]

------------------------------------------------------------------------

exitModal :: ModalMaker
exitModal swd = (wd, ["", padl <> "Exit (y)?", ""]) where
    wd = commonModalWidth swd `min` 19
    padl = P.replicate ((wd - 9) `div` 2) ' '

------------------------------------------------------------------------

-- | The time used and time left
pTimes :: DrawData -> StringA
pTimes DD { drawFrame=Just Frame {..}, drawSize=Size{sizeW=x} } =
    FancyS $ map (, defaultSty)
        if x - 4 < P.length elapsed
        then [" "]
        else ["  ", elapsed]
                ++ (guard (distance > 0) *> [gap, remaining])
  where
    elapsed   = P.pack $ printf "%d:%02d" l_m l_s
    remaining = P.pack $ printf "-%d:%02d" r_m r_s
    (l_m, l_s) = toMS currentTime
    (r_m, r_s) = toMS timeLeft
    gap        = spaces distance
    distance   = x - 4 - P.length elapsed - P.length remaining
    toMS :: RealFrac a => a -> (Int, Int)
    toMS = flip quotRem 60 . floor
pTimes _ = Fast (spaces 5) defaultSty

------------------------------------------------------------------------

-- | A progress bar
progressBar :: DrawData -> StringA
progressBar dd@DD{drawSize=Size{sizeW=w}, drawState=st} = case drawFrame dd of
    Nothing -> FancyS [("  ", defaultSty), (spaces (w-4), bgs)]
      where
        Style _ bg = progress (config st)
        bgs        = Style bg bg
    Just Frame {..} -> FancyS
        [ ("  ", defaultSty)
        , (spaces distance, fgs)
        , (spaces (width - distance), bgs) ]
      where
        width    = w - 4
        total    = curr + left
        distance = round ((curr / total) * fromIntegral width)
        curr     = realToFrac currentTime :: Float
        left     = realToFrac timeLeft
        Style fg bg = progress (config st)
        bgs         = Style bg bg
        fgs         = Style fg fg

------------------------------------------------------------------------

-- | Version info
pVersion :: ByteString
pVersion = P.pack versinfo

-- | Uptime
pTime :: DrawData -> ByteString
pTime = uptime . drawState

-- | Play mode
pMode :: DrawData -> String
pMode dd = case status (drawState dd) of
    Stopped -> "◼"
    Paused  -> "⏸"
    Playing -> "▶"

-- | Loop, normal, or random
pMode2 :: DrawData -> String
pMode2 dd = case mode (drawState dd) of
    Random -> "rand"
    Loop   -> "loop"
    Once   -> "once"
    Single -> "sing"

------------------------------------------------------------------------

-- | The two play-mode glyphs (e.g. "▶ rand") rendered together.
playModes :: DrawData -> String
playModes dd = pMode dd ++ ' ' : pMode2 dd

-- | "x/n dir(s)  y/m file(s)" cursor position read-out.
playInfo :: DrawData -> ByteString
playInfo dd = mconcat
    -- TODO pregenerate as template
    [ spaces (P.length numd - P.length curd)
    , curd, "/", numd, " dir", onPlural (snd . bounds $ folders st) "" "s"
    , " "
    , spaces (P.length numf - P.length curf)
    , curf, "/", numf, " file", onPlural (size st) "" "s"
    ]
  where
    st   = drawState dd
    tobs = P.pack . show
    onPlural 1 s _ = s
    onPlural _ _ p = p
    curf  = tobs $ 1 + cursor st
    numf  = tobs $ size st
    mydir = fdir $ music st ! cursor st
    curd  = tobs $ 1 + mydir
    numd  = tobs $ 1 + snd (bounds $ folders st)

-- | The top title bar: cursor position + play modes + uptime + version.
playTitle :: DrawData -> StringA
playTitle dd =
    FancyS $ map (, hl)
        if gap >= 2
        then [mconcat [" ", inf, spaces gapl], modesBS,
                mconcat [spaces gapr, time, " ", ver, " "]]
        else let gap' = x - modlen; gapl' = gap' `div` 2
             in if gap' >= 2
                then [spaces gapl', modesBS, spaces $ gap' - gapl']
                else [" ", u $ take (x-2) modes, " "]
  where
    inf     = playInfo dd
    time    = pTime dd
    modes   = playModes dd
    ver     = pVersion
    modesBS = u modes

    x       = sizeW $ drawSize dd
    lsize   = 1 + P.length inf
    rsize   = 2 + P.length time + P.length ver
    side    = (x - modlen) `div` 2
    gap     = x - modlen - lsize - rsize
    gapl    = 1 `max` ((side - lsize) `min` (gap - 1))
    gapr    = gap - gapl
    modlen  = 6 -- length modes
    hl      = titlebar . config $ drawState dd

-- | The scrolling playlist (title + visible tracks + minibuffer).
playList :: DrawData -> [StringA]
playList dd@DD{ drawSize=Size y x, drawPos=Pos{posY=o}, drawState=st } =
    playTitle dd
    : list
    ++ replicate (height - length list - 2) (Fast P.empty defaultSty)
    ++ [minibuffer st]
  where
    songs  = music st
    this   = current st
    curr   = cursor  st
    height = y - o

    -- number of screens down, and then offset
    buflen = height - 2
    (screens, select) = quotRem curr buflen -- keep cursor in screen

    playing = let top = screens * buflen
                  bot = (screens + 1) * buflen
              in if this >= top && this < bot
                    then this - top -- playing song is visible
                    else (-1)

    -- visible slice of the playlist
    visible = slice off (off + buflen) songs
        where off = screens * buflen

    visible' :: [(Maybe Int, ByteString)]
    visible' = loop (-1) visible where
        loop _ []     = []
        loop n (v:vs) =
            let r = if fdir v > n then Just (fdir v) else Nothing
            in (r, toMaxWidth (x - indent - 1) $ fbase v)
                    : loop (fdir v) vs

    list   = [ drawIt . color $ n | n <- zip visible' [0..] ]

    indent = (round $ (0.334 :: Float) * fromIntegral x) :: Int

    color :: ((Maybe Int, ByteString), Int)
                -> (Maybe Int, Style, [ByteString])
    color ((m, s), i)
        | i == select && i == playing = f sty3
        | i == select                 = f sty2
        | i == playing                = f sty1
        | otherwise                   = (m, defaultSty, [s])
        where
            f sty = (m, sty,
                [s, spaces (x - indent - 1 - displayWidth s)])

    sty1 = selected . config $ st
    sty2 = cursors  . config $ st
    sty3 = combined . config $ st

    drawIt :: (Maybe Int, Style, [ByteString]) -> StringA
    drawIt (Nothing, sty, v) =
        FancyS $ map (, sty) $ spaces (1 + indent) : v

    drawIt (Just i, sty, v) = FancyS
        $ (d, sty')
        : (spaces (indent + 1 - displayWidth d), sty')
        : map (, sty) v
      where
        sty' = if sty == sty2 || sty == sty3 then sty2 else sty1
        d = toMaxWidth (indent - 1) $ takeFileName
                $ case size st of
                    0 -> "(empty)"
                    _ -> dname $ folders st ! i

------------------------------------------------------------------------

spaces :: Int -> ByteString
spaces = flip P.replicate ' '

------------------------------------------------------------------------
-- | Now write out just the clock line
redrawJustClock :: Draw
redrawJustClock = Draw $ discardErrors do
    st     <- getsHS id
    (h, w) <- screenSize
    let dd = DD (Size h w) undefined st (clock st)
    Curses.wMove Curses.stdScr 1 0   -- hardcoded!
    drawLine $ progressBar dd
    Curses.wMove Curses.stdScr 2 0   -- hardcoded!
    drawLine $ pTimes dd

------------------------------------------------------------------------
-- | General modal renderer.
renderModal :: HState -> Size -> ModalMaker -> IO ()
renderModal st (Size h w) mkr = do
    let (mw, modal') = mkr w
        hoffset = max 0 $ (w - mw) `div` 2
        vislines = (h - 5) `min` length modal'
        voffset = ((h - vislines) `div` 2) `max` 4
        sty = modals $ config st
    Curses.wMove Curses.stdScr voffset hoffset
    for_ (take vislines modal') \t -> do
        drawLine $ Fast (toWidth mw t) sty
        (y', _) <- Curses.getYX Curses.stdScr
        Curses.wMove Curses.stdScr (y'+1) hoffset

-- | Choose modal to render based on state.
renderModals :: HState -> Size -> IO ()
renderModals st sz =
    whenJust (modal st) $ renderModal st sz . \case
        HelpModal h -> helpModal h
        HistModal h -> histModal h
        ExitModal   -> exitModal

------------------------------------------------------------------------
--
-- | Draw the screen
--
redraw :: Draw
redraw = Draw $ discardErrors {- TODO what errors are discarded? -} do
    st <- getsHS id    -- another refresh could be triggered?
    (h, w) <- screenSize
    let sz     = Size h w
        screen = playScreen (DD sz (Pos 0 0) st (clock st))
        a      = screen ++ playList (DD sz (Pos (length screen) 0) st (clock st))

    setXterm st

    gotoTop
    for_ (take (h-1) (init a)) \t -> do
        drawLine t
        (y, x) <- Curses.getYX Curses.stdScr
        fillLine
        maybeLineDown t h y x
    renderModals st sz

    -- minibuffer
    Curses.wMove Curses.stdScr (h-1) 0
    fillLine
    Curses.wMove Curses.stdScr (h-1) 0
    drawLine (last a)
    when (miniFocused st) do -- a fake cursor
        drawLine (Fast (spaces 1) (blockcursor . config $ st ))
        -- XXX is this TODO from 2005 still relevant?
        -- todo rendering bug here when deleting backwards in minibuffer

------------------------------------------------------------------------
--
-- | Draw a coloured (or not) string to the screen
--
drawLine :: StringA -> IO ()
drawLine (Fast ps sty) = drawSegment ps sty
drawLine (FancyS ls)   = traverse_ (uncurry drawSegment) ls

-- | Write a single styled UTF-8 segment.  Safe because C only reads the bytes.
drawSegment :: ByteString -> Style -> IO ()
drawSegment bs sty = withStyle sty $ void $
    P.unsafeUseAsCStringLen bs \(cstr, len) ->
        waddnstr Curses.stdScr cstr (fromIntegral len)


------------------------------------------------------------------------

maybeLineDown :: StringA -> Int -> Int -> Int -> IO ()
maybeLineDown (Fast s _) h y _ | s == P.empty = lineDown h y
maybeLineDown _ h y x
    | x == 0    = pure ()     -- already moved down
    | otherwise = lineDown h y

------------------------------------------------------------------------

lineDown :: Int -> Int -> IO ()
lineDown h y = Curses.wMove Curses.stdScr (min h (y+1)) 0

--
-- | Fill to end of line spaces
--
fillLine :: IO ()
fillLine = discardErrors Curses.clrToEol -- harmless?

--
-- | move cursor to origin of stdScr.
--
gotoTop :: IO ()
gotoTop = Curses.wMove Curses.stdScr 0 0
{-# INLINE gotoTop #-}

-- | Take a slice of an array efficiently
slice :: Int -> Int -> Array Int e -> [e]
slice i j arr = 
    let (a, b) = bounds arr
    in [unsafeAt arr n | n <- [max a i .. min b j] ]
{-# INLINE slice #-}

------------------------------------------------------------------------

--
-- | magics for setting xterm titles using ansi escape sequences
--
setXtermTitle :: [ByteString] -> IO ()
setXtermTitle strs = do
    traverse_ (P.hPut stderr) (before : strs ++ [after])
    hFlush stderr 
  where
    before = "\ESC]0;"
    after  = "\007"

------------------------------------------------------------------------

-- set xterm title.  Don't need to do this on each refresh...
setXterm :: HState -> IO ()
setXterm s = setXtermTitle $ case status s of
    Playing -> case id3 s of
        Nothing -> [fbase $ music s ! current s]
        Just ti -> id3artist ti :
                   if P.null (id3title ti)
                        then []
                        else [": ", id3title ti]
    Paused  -> ["paused"]
    Stopped -> ["stopped"]


foreign import ccall safe
    waddnstr :: Curses.Window -> CString -> CInt -> IO CInt

