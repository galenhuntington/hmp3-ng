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
    getKey,
    -- * Tool
    u,
  ) where

import Base
import Elements as El
import Style
import Playlist                 (File(fdir, fbase), Dir(dname))
import State
import Decoder
import Text                     (u, displayWidth, toMaxWidth, toWidth, spaces)
import UI.HSCurses.Curses qualified as Curses
import Keyboard                 (unkey)

import Data.Array               ((!), bounds, Array)
import Data.Array.Base          (unsafeAt)
import System.Posix.FilePath    (takeFileName)
import System.IO                (stderr, hFlush)
import System.Posix.Signals     (installHandler, Handler(..))

import Foreign.C.String
import Foreign.C.Types
import Foreign.C.Error (Errno(..), getErrno)

import Data.ByteString.Char8 qualified as P
import Data.ByteString.Unsafe qualified as P


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
    let sty = if colorify then defaultStyle else monoStyle

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

-- | Rewrite of Curses.getCh to avoid looping on terminal crash
-- | (also no unget support since I don't need it)
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

-- | Read a key. UIs need to define a method for getting events.
-- We only need to refresh if we don't have no SIGWINCH support.
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
data Size = Size { _sizeH, sizeW :: !Int }

-- | Renderable widgets are functions @DrawData -> ...@.
data DrawData = DD {
    drawSize  :: Size,
    drawState :: HState
    }

------------------------------------------------------------------------

-- | The three lines of the play info widget.
playScreen :: DrawData -> [StringA]
playScreen dd = [pPlaying dd, progressBar dd, pTimes dd]

------------------------------------------------------------------------

-- | Info about the current track
pPlaying :: DrawData -> StringA
pPlaying dd = flip Fast defaultSty $ "  " <> mconcat line where
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
    Nothing -> fbase $ music st ! current st

-- | mp3 information
pInfo :: DrawData -> ByteString
pInfo DD{drawState=st} = fromMaybe "" $ info st

------------------------------------------------------------------------

-- | The time used and time left
pTimes :: DrawData -> StringA
pTimes DD { drawState=st, drawSize=Size{sizeW=w} } =
    flip Fast defaultSty $ if w - 4 < P.length elapsed
        then ""
        else mconcat $ ["  ", elapsed] ++ [gap <> "-" <> remaining | distance > 0]
  where
    elapsed   = showClock (maybe 0 (.currentTime) st.clock)
    remaining = maybe "?:??.?" (showClock . (.timeLeft)) st.clock
    gap       = spaces distance
    distance  = w - 5 - P.length elapsed - P.length remaining

------------------------------------------------------------------------

-- | A progress bar
progressBar :: DrawData -> StringA
progressBar DD{drawSize=Size{sizeW}, drawState=st} = case st.clock of
    Nothing         -> FancyS [("  ", defaultSty), (spaces width, bgs)]
    Just Frame {..} -> FancyS
        [ ("  ", defaultSty)
        , (spaces distance, fgs)
        , (spaces (width - distance), bgs) ]
      where
        total    = curr + toRational timeLeft - ε
        distance = ceiling (curr * fromIntegral (width - 1) / total)
        curr     = toRational currentTime
        ε        = toRational (succ 0 `asTypeOf` currentTime) / 2
  where
    width       = sizeW - 4
    Style fg bg = progress st.uiStyle
    bgs         = Style bg bg
    fgs         = Style fg fg

------------------------------------------------------------------------

-- | Uptime
pTime :: DrawData -> ByteString
pTime = uptime . drawState

-- | Play state
pState :: DrawData -> String
pState dd = case status (drawState dd) of
    Stopped -> "◼"
    Paused  -> "⏸"
    Playing -> "▶"

-- | Play mode
pMode :: DrawData -> String
pMode dd = take 4 $ map toLower $ show $ mode $ drawState dd

------------------------------------------------------------------------

-- | "x/n dirs y/m files" cursor position read-out.
playInfo :: DrawData -> ByteString
playInfo dd = mconcat
    [ spaces (P.length numd - P.length curd)
    , curd, "/", numd, " dirs"
    , spaces (1 + P.length numf - P.length curf)
    , curf, "/", numf, " files"
    ]
  where
    st   = drawState dd
    tobs = P.pack . show
    curf  = tobs $ 1 + st.cursor
    numf  = tobs $ st.size
    mydir = fdir $ st.music ! st.cursor
    curd  = tobs $ 1 + mydir
    numd  = tobs $ length $ st.folders

-- | The top title bar: cursor position + play indicator + uptime + version.
playTitle :: DrawData -> StringA
playTitle dd =
    flip Fast hl $ mconcat if gap >= 2
        then [" ", inf, spaces gapl, u indic, spaces gapr, time, " ", ver, " "]
        else let gap' = x - indicl; gapl' = gap' `div` 2
             in if gap' >= 2
                then [spaces gapl', u indic, spaces $ gap' - gapl']
                else [" ", u $ take (x-2) indic, " "]
  where
    inf     = playInfo dd
    time    = pTime dd
    indic   = pState dd ++ ' ' : pMode dd
    ver     = El.pVersion
    x       = sizeW $ drawSize dd
    lsize   = 1 + P.length inf
    rsize   = 2 + P.length time + P.length ver
    side    = (x - indicl) `div` 2
    gap     = x - indicl - lsize - rsize
    gapl    = 1 `max` ((side - lsize) `min` (gap - 1))
    gapr    = gap - gapl
    indicl  = 6 -- length indic
    hl      = titlebar . uiStyle $ drawState dd

-- | The scrolling playlist (title + visible tracks + minibuffer).
playList :: Int -> DrawData -> [StringA]
playList height dd@DD{ drawSize=Size _ x, drawState=st } =
    playTitle dd
    : list
    ++ replicate (height - length list - 2) (Fast P.empty defaultSty)
    ++ [minibuffer st]
  where
    songs  = music st
    this   = current st
    curr   = cursor  st

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

    (sty1, sty2, sty3) = (selected cs, cursors cs, combined cs)
        where cs = uiStyle st

    color :: ((Maybe Int, ByteString), Int)
                -> (Maybe Int, (Style, [ByteString]))
    color ((m, s), i) = (m,) case (i == select, i == playing) of
        (True, True) -> f sty3
        (True, _)    -> f sty2
        (_   , True) -> f sty1
        _            -> (defaultSty, [s])
      where
        f sty = (sty, [s, spaces (x - indent - 1 - displayWidth s)])

    drawIt :: (Maybe Int, (Style, [ByteString])) -> StringA
    drawIt (Nothing, (sty, v)) =
        FancyS $ map (, sty) $ spaces (1 + indent) : v
    drawIt (Just i, (sty, v)) = FancyS
        $ (d, sty')
        : (spaces (indent + 1 - displayWidth d), sty')
        : map (, sty) v
      where
        sty' = if sty == sty2 || sty == sty3 then sty2 else sty1
        d = toMaxWidth (indent - 1) $ takeFileName $ dname $ folders st ! i

------------------------------------------------------------------------
-- | Now write out just the clock line
redrawJustClock :: Draw
redrawJustClock = Draw $ discardErrors do
    st     <- getsHS id
    (h, w) <- screenSize
    let dd = DD (Size h w) st
    Curses.wMove Curses.stdScr 1 0
    drawLine $ progressBar dd
    Curses.wMove Curses.stdScr 2 0
    drawLine $ pTimes dd

------------------------------------------------------------------------
-- | General modal renderer.
renderModal :: HState -> Size -> ModalMaker -> IO ()
renderModal st (Size h w) mkr = do
    let (mw, modal') = mkr w
        hoffset = max 0 $ (w - mw) `div` 2
        vislines = (h - 5) `min` length modal'
        voffset = ((h - vislines) `div` 2) `max` 4
        sty = modals $ uiStyle st
    Curses.wMove Curses.stdScr voffset hoffset
    for_ (take vislines modal') \t -> do
        drawLine $ Fast (toWidth mw t) sty
        (y', _) <- Curses.getYX Curses.stdScr
        Curses.wMove Curses.stdScr (y'+1) hoffset

-- | Choose modal to render based on state.
renderModals :: HState -> Size -> IO ()
renderModals st sz =
    whenJust (modal st) $ renderModal st sz . \case
        HelpModal h -> El.helpModal h
        HistModal h -> El.histModal h
        ExitModal   -> El.exitModal

------------------------------------------------------------------------
-- | Draw the screen
redraw :: Draw
redraw = Draw $ discardErrors {- TODO what errors are discarded? -} do
    st <- getsHS id    -- another refresh could be triggered?
    (h, w) <- screenSize
    let sz     = Size h w
        screen = playScreen (DD sz st)
        a      = screen ++ playList (h - length screen) (DD sz st)

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
        drawLine (Fast (spaces 1) (blockcursor . uiStyle $ st ))
        -- XXX is this TODO from 2005 still relevant?
        -- todo rendering bug here when deleting backwards in minibuffer

------------------------------------------------------------------------
-- | Draw a coloured (or not) string to the screen
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

-- | Fill to end of line spaces
fillLine :: IO ()
fillLine = discardErrors Curses.clrToEol -- harmless?

-- | move cursor to origin of stdScr.
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

-- | magics for setting xterm titles using ansi escape sequences
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

