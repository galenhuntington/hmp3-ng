-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- Derived from: riot/UI.hs Copyright (c) Tuomo Valkonen 2004.
-- Released under the same license.

-- | This module defines a user interface implemented using ncurses. 

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
import Text                     (u, displayWidth, toMaxWidth, toWidth, spaces, showInt)
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
resizeui :: Draw
resizeui = Draw do
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
    void Curses.scrSize

refresh :: IO ()
refresh = runDraw $ redraw <> Draw Curses.refresh

refreshClock :: IO ()
refreshClock = runDraw $ redrawJustClock <> Draw Curses.refresh

------------------------------------------------------------------------

data DrawData = DD { drawWidth :: Int, drawState :: HState }

------------------------------------------------------------------------

-- | Info about the current track
pPlaying :: DrawData -> Line
pPlaying dd = pure $ plainSeg $ "  " <> mconcat line where
    x = dd.drawWidth
    a = pId3 dd
    b = fromMaybe "" dd.drawState.info  -- mp3 info
    line | gap >= 0 = a : spaces gap : right
         | True     = toMaxWidth lim a : right
        where lim = x - 5 - (if showId3 then P.length b else -1)
              gap = lim - displayWidth a
              showId3 = x > 59
              right = if showId3 then [" ", b] else []

-- | Id3 info
pId3 :: DrawData -> ByteString
pId3 DD{drawState=st} = maybe (st.music ! st.current).fbase (.str) st.id3

------------------------------------------------------------------------

-- | Show progress bar.
progressBar :: DrawData -> Line
progressBar (DD w st) = [
    plainSeg "  ", Seg (Style fg fg) (spaces x), Seg sty (spaces (w'-x)) ]
  where
    w' = w - 4
    x = El.progress w' st.clock
    sty@(Style fg _) = st.uiStyle.progress

-- | Two lines showing clock.
clockLines :: DrawData -> [Line]
clockLines dd@(DD w st) = [progressBar dd, [plainSeg (El.pTimes w st.clock)]]

------------------------------------------------------------------------

-- | Play state
pState :: DrawData -> String
pState dd = case dd.drawState.status of
    Stopped -> "◼"
    Paused  -> "⏸"
    Playing -> "▶"

-- | Play mode
pMode :: DrawData -> String
pMode dd = take 4 $ map toLower $ show dd.drawState.mode

------------------------------------------------------------------------

-- | "x/n dirs y/m files" cursor position read-out.
playInfo :: DrawData -> ByteString
playInfo DD{drawState=st} = mconcat
    [ spaces (P.length numd - P.length curd)
    , curd, "/", numd, " dirs"
    , spaces (1 + P.length numf - P.length curf)
    , curf, "/", numf, " files"
    ]
  where
    curf  = showInt $ st.cursor + 1
    numf  = showInt $ st.size
    curd  = showInt $ (st.music ! st.cursor).fdir + 1
    numd  = showInt $ length $ st.folders

-- | The top title bar: cursor position + play indicator + uptime + version.
playTitle :: DrawData -> Line
playTitle dd@DD{drawWidth=w, drawState=st} =
    [Seg st.uiStyle.titlebar $ El.layoutLCR w (left, centerS, right)]
  where
    left    = " " <> playInfo dd
    centerS = pState dd ++ ' ' : pMode dd  -- always 6 chars
    right   = st.uptime <> " " <> El.pVersion <> " "

-- | The scrolling playlist (visible tracks).
playList :: Int -> DrawData -> [Line]
playList buflen _ | buflen <= 0 = []  -- extra defense besides laziness
playList buflen DD{ drawWidth=w, drawState=st } =
    list ++ replicate (buflen - length list) []

  where
    -- number of screens down, and then offset
    (screens, select) = st.cursor `quotRem` buflen -- keep cursor in screen
    playing = st.current - screens * buflen -- invisible if out of bounds

    -- visible slice of the playlist
    visible = slice off (off + buflen - 1) st.music
        where off = screens * buflen

    visible' :: [(Maybe Int, ByteString)]
    visible' = loop (-1) visible where
        loop _ []     = []
        loop n (v:vs) =
            let r = if v.fdir > n then Just v.fdir else Nothing
            in (r, toMaxWidth (w - indent - 1) v.fbase) : loop v.fdir vs

    list   = [ drawIt . color $ n | n <- zip visible' [0..] ]

    indent = round $ (0.334 :: Float) * fromIntegral w :: Int

    (sty1, sty2, sty3) = (cs.selected, cs.cursors, cs.combined)
        where cs = st.uiStyle

    color :: ((Maybe Int, ByteString), Int)
                -> (Maybe Int, (Style, [ByteString]))
    color ((m, s), i) = (m,) case (i == select, i == playing) of
        (True, True) -> f sty3
        (True, _)    -> f sty2
        (_   , True) -> f sty1
        _            -> (defaultSty, [s])
      where
        f sty = (sty, [s, spaces (w - indent - 1 - displayWidth s)])

    drawIt :: (Maybe Int, (Style, [ByteString])) -> Line
    drawIt (Nothing, (sty, v)) =
        map (Seg sty) $ spaces (1 + indent) : v
    drawIt (Just i, (sty, v)) = Seg sty' d
        : Seg sty' (spaces (indent + 1 - displayWidth d))
        : map (Seg sty) v
      where
        sty' = if sty == sty2 || sty == sty3 then sty2 else sty1
        d = toMaxWidth (indent - 1) $ takeFileName (st.folders ! i).dname

------------------------------------------------------------------------
-- | Write out only the clock lines.
redrawJustClock :: Draw
redrawJustClock = Draw $ discardErrors do
    st <- getsHS id
    (h, w) <- screenSize
    drawFullLines (h-1) 1 $ clockLines $ DD w st

------------------------------------------------------------------------
-- | General modal renderer.
renderModal :: HState -> (Int, Int) -> ModalMaker -> IO ()
renderModal st (h, w) mkr = do
    let (mw, modal') = mkr w
        hoffset = max 0 $ (w - mw) `div` 2
        vislines = (h - 5) `min` length modal'
        voffset = ((h - vislines) `div` 2) `max` 4
    Curses.wMove Curses.stdScr voffset hoffset
    for_ (take vislines modal') \t -> do
        drawSegment $ Seg st.uiStyle.modals (toWidth mw t)
        (y', _) <- Curses.getYX Curses.stdScr
        Curses.wMove Curses.stdScr (y'+1) hoffset

-- | Choose modal to render based on state.
renderModals :: HState -> (Int, Int) -> IO ()
renderModals st sz =
    whenJust st.modal $ renderModal st sz . \case
        HelpModal h -> El.helpModal h
        HistModal h -> El.histModal h
        ExitModal   -> El.exitModal

------------------------------------------------------------------------
-- | Draw the screen
-- Errors can maybe be thrown if screen gets resized mid-render.
redraw :: Draw
redraw = Draw $ discardErrors do
    st <- getsHS id
    sz@(h, w) <- screenSize
    setXterm st
    drawFullLines (h-1) 0 $ let dd = DD w st in
        pPlaying dd : clockLines dd ++ playTitle dd : playList (h-5) dd
    renderModals st sz
    -- minibuffer
    Curses.wMove Curses.stdScr (h-1) 0
    drawLine st.minibuffer
    when st.miniFocused do -- a fake cursor
        drawSegment $ Seg st.uiStyle.blockcursor " "
    fillLine

-- | Render whole lines without going below limit.
drawFullLines :: Int -> Int -> [Line] -> IO ()
drawFullLines limit y ls =
    for_ (zip [y .. limit-1] ls) \ (y', t) -> do
        Curses.wMove Curses.stdScr y' 0
        drawLine t *> fillLine

------------------------------------------------------------------------
-- | Draw a coloured (or not) string to the screen
drawLine :: Line -> IO ()
drawLine = traverse_ drawSegment

-- | Write a single styled UTF-8 segment.  Safe because C only reads the bytes.
drawSegment :: Segment -> IO ()
drawSegment (Seg sty bs) = withStyle sty $ void $
    P.unsafeUseAsCStringLen bs \(cstr, len) ->
        waddnstr Curses.stdScr cstr (fromIntegral len)

------------------------------------------------------------------------

-- | Fill to end of line spaces
-- (Curses throws error if already at end.)
fillLine :: IO ()
fillLine = discardErrors Curses.clrToEol

-- | Take a slice of an array efficiently
slice :: Int -> Int -> Array Int e -> [e]
slice i j arr = 
    let (a, b) = bounds arr
    in [unsafeAt arr n | n <- [max a i .. min b j]]
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
setXterm st = setXtermTitle case st.status of
    Playing -> case st.id3 of
        Just id3 -> id3.artist :
                       if P.null id3.title then [] else [": ", id3.title]
        _        -> [(st.music ! st.current).fbase]
    Paused  -> ["paused"]
    Stopped -> ["stopped"]


foreign import ccall safe
    waddnstr :: Curses.Window -> CString -> CInt -> IO CInt

