{-# LANGUAGE ForeignFunctionInterface, TupleSections, AllowAmbiguousTypes #-}

-- 
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019, 2020 Galen Huntington
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
-- Derived from: riot/UI.hs
--      Copyright (c) Tuomo Valkonen 2004.
--
-- Released under the same license.
--

--
-- | This module defines a user interface implemented using ncurses. 
--
--

module UI (
        runDraw,

        -- * Construction, destruction
        start, end, suspend, screenSize, refresh, refreshClock, resetui,

        -- * Input
        getKey

  )   where

import Base

import Style
import FastIO                   (basenameP)
import Tree                     (File(fdir, fbase), Dir(dname))
import State
import Syntax
import Config
import qualified UI.HSCurses.Curses as Curses
import {-# SOURCE #-} Keymap    (extraTable, keyTable, unkey, charToKey)

import Data.Array               ((!), bounds, Array, listArray)
import Data.Array.Base          (unsafeAt)
import System.IO                (stderr, hFlush)
import System.Posix.Signals     (raiseSignal, sigTSTP, installHandler, Handler(..))

import Foreign.C.String
import Foreign.C.Types

import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8


newtype Draw = Draw (IO ())
instance Semigroup Draw where Draw x <> Draw y = Draw $ x >> y
instance Monoid Draw where mempty = Draw $ pure ()

runDraw :: Draw -> IO ()
runDraw (Draw d) = withDrawLock d

------------------------------------------------------------------------

--
-- | how to initialise the ui
--
start :: IO UIStyle
start = do
    discardErrors do
        thisterm <- lookupEnv "TERM"
        case thisterm of 
            Just "vt220" -> setEnv "TERM" "xterm-color"
            Just t | "xterm" `isPrefixOf` t 
                   -> silentlyModifyST $ \st -> st { xterm = True }
            _ -> pure ()

    Curses.initCurses
    case Curses.cursesSigWinch of
        Just wch -> void $ installHandler wch (Catch resetui) Nothing
        _        -> pure () -- handled elsewhere

    colorify <- Curses.hasColors
    light    <- isLightBg

    let sty | colorify && light = lightBgStyle
            | colorify          = defaultStyle
            | otherwise         = bwStyle 

    initcolours sty
    Curses.keypad Curses.stdScr True    -- grab the keyboard
    runDraw nocursor

    pure sty

-- | Reset
resetui :: IO ()
resetui = runDraw (resizeui <> nocursor) >> refresh

-- | And force invisible
nocursor :: Draw
nocursor = Draw $ discardErrors $ void $ Curses.cursSet Curses.CursorInvisible

--
-- | Clean up and go home. Refresh is needed on linux. grr.
--
end :: Bool -> IO ()
end isXterm = do when isXterm $ setXtermTitle ["xterm"]
                 Curses.endWin

--
-- | Suspend the program
--
suspend :: IO ()
suspend = raiseSignal sigTSTP

--
-- | Find the current screen height and width.
--
screenSize :: IO (Int, Int)
screenSize = Curses.scrSize

--
-- | Read a key. UIs need to define a method for getting events.
-- We only need to refresh if we don't have no SIGWINCH support.
--
getKey :: IO Char
getKey = do
    k <- Curses.getCh
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
        Curses.leaveOk True
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

--
-- | Some evil to work out if the background is light, or dark. Assume dark.
--
isLightBg :: IO Bool
isLightBg = handle @SomeException (\_ -> pure False) do
    e <- getEnv "HMP_HAS_LIGHT_BG"
    pure $ map toLower e == "true"

------------------------------------------------------------------------

-- (prefix some with underscore to avoid unused warnings)
data Pos  = Pos  { posY, _posX :: !Int }
data Size = Size { _sizeH, sizeW :: !Int }

data DrawData = DD {
    drawSize  :: Size,
    drawPos   :: Pos,
    drawState :: HState,
    drawFrame :: Maybe Frame
    }

--
-- | A class for renderable objects, given the application state, and
-- for printing the object as a list of strings
--
class Element a where draw :: DrawData -> a

--
-- | The elements of the play mode widget
--
data PlayScreen = PlayScreen !PPlaying !ProgressBar !PTimes 

-- | How does this all work? Firstly, we mostly want to draw fast strings
-- directly to the screen. To break the drawing problem down, you need
-- to write an instance of Element for each element in the ui. Larger
-- and larger elements then combine these items together. 
--
-- Obviously to write the element instance, you need a new type for each
-- element, to disinguish them. As follows:

newtype PlayList = PlayList [StringA]

newtype PPlaying    = PPlaying    StringA
newtype PVersion    = PVersion    ByteString
newtype PMode       = PMode       String
newtype PMode2      = PMode2      String
newtype ProgressBar = ProgressBar StringA
newtype PTimes      = PTimes      StringA

newtype PInfo       = PInfo       ByteString
newtype PId3        = PId3        ByteString
newtype PTime       = PTime       ByteString

newtype PlayTitle = PlayTitle StringA
newtype PlayInfo  = PlayInfo  ByteString
newtype PlayModes = PlayModes String

data HelpScreen
data HistScreen
class ModalElement a where drawModal :: DrawData -> Maybe [StringA]

------------------------------------------------------------------------

instance Element PlayScreen where
    draw dd = PlayScreen (draw dd) (draw dd) (draw dd)

-- | Decode the play screen
printPlayScreen :: PlayScreen -> [StringA]
printPlayScreen (PlayScreen (PPlaying a) 
                            (ProgressBar b) 
                            (PTimes c)) = [a , b , c]

------------------------------------------------------------------------

instance (Element a, Element b) => Element (a,b) where
    draw dd = (draw dd, draw dd)

------------------------------------------------------------------------

-- Info about the current track
instance Element PPlaying where
    draw dd =
        PPlaying . FancyS $ map (, defaultSty) $ spc2 : line
      where
        x       = sizeW $ drawSize dd
        PId3 a  = draw dd
        PInfo b = draw dd
        s       = UTF8.toString a
        line | gap >= 0 = [U s, B $ spaces gap] ++ right
             | True     = [U $ ellipsize lim s] ++ right
            where lim = x - 5 - (if showId3 then P.length b else -1)
                  gap = lim - displayWidth s
                  showId3 = x > 59
                  right = if showId3 then [B " ", B b] else []

-- | Id3 Info
instance Element PId3 where
    draw DD{drawState=st} = case id3 st of
        Just i  -> PId3 $ id3str i
        Nothing -> PId3 $ case size st of
                                0 -> emptyVal
                                _ -> fbase $ music st ! current st

-- | mp3 information
instance Element PInfo where
    draw DD{drawState=st} = PInfo case info st of
        Nothing  -> emptyVal
        Just i   -> userinfo i

emptyVal :: ByteString
emptyVal = "(empty)"

spc2 :: AmbiString
spc2 = B $ spaces 2

modalWidth :: Int -> Int
modalWidth w = max (min w 3) $ round $ fromIntegral w * (0.8::Float)

------------------------------------------------------------------------

-- instance ModalElement me => Element (Modal me) where draw = drawModal

instance ModalElement HelpScreen where
    drawModal DD{drawSize=Size{sizeW=w}, drawState=st} = do
        guard $ helpVisible st
        pure $
            [ Fast (f cs h) sty | (h,cs,_) <- keyTable ] ++
            [ Fast (f cs h) sty | (h,cs) <- extraTable ]
        where
            sty  = modal . config $ st

            f :: [Char] -> ByteString -> ByteString
            f cs ps =
                let p = str <> ps
                    rt = tot - P.length p
                in if rt > 0
                    then p <> spaces rt
                    else P.take (tot - 1) p <> UTF8.fromString "…"
                where
                    tot = modalWidth w
                    len = max 2 $ round $ fromIntegral tot * (0.2::Float)

                    str = P.take len $ P.intercalate " "
                        ([""] ++ map pprIt cs ++ [P.replicate len ' '])

                    pprIt c = case c of
                          '\n'            -> "Enter"
                          '\f'            -> "^L"
                          '\\'            -> "\\"
                          ' '             -> "Space"
                          _ -> case charToKey c of
                            Curses.KeyUp    -> "Up"
                            Curses.KeyDown  -> "Down"
                            Curses.KeyPPage -> "PgUp"
                            Curses.KeyNPage -> "PgDn"
                            Curses.KeyLeft  -> "Left"
                            Curses.KeyRight -> "Right"
                            Curses.KeyEnd   -> "End"
                            Curses.KeyHome  -> "Home"
                            Curses.KeyBackspace -> "Backspace"
                            _ -> P.singleton c

------------------------------------------------------------------------

instance ModalElement HistScreen where
    drawModal DD{drawSize=Size{sizeW=w}, drawState=st} =
        fmap (\_ -> [Fast "test" $ modal . config $ st]) $ histVisible st
        -- [ Fast (f cs h) sty | (h,cs,_) <- keyTable ]

------------------------------------------------------------------------

-- | The time used and time left
instance Element PTimes where
    draw DD { drawFrame=Just Frame {..}, drawSize=Size{sizeW=x} } =
        PTimes $ FancyS $ map (, defaultSty)
            if x - 4 < P.length elapsed
            then [B " "]
            else [spc2, B elapsed]
                    ++ (guard (distance > 0) *> [B gap, B remaining])
      where
        elapsed   = P.pack $ printf "%d:%02d" l_m l_s
        remaining = P.pack $ printf "-%d:%02d" r_m r_s
        (l_m,l_s) = toMS currentTime
        (r_m,r_s) = toMS timeLeft
        gap       = spaces distance
        distance  = x - 4 - P.length elapsed - P.length remaining
        toMS :: RealFrac a => a -> (Int, Int)
        toMS = flip quotRem 60 . floor
    draw _ = PTimes $ Fast (spaces 5) defaultSty

------------------------------------------------------------------------

-- | A progress bar
instance Element ProgressBar where
    draw dd@DD{drawSize=Size{sizeW=w}, drawState=st} = case drawFrame dd of
      Nothing -> ProgressBar . FancyS $
              [(spc2,defaultSty) ,(B $ spaces (w-4), bgs)]
        where 
          (Style _ bg) = progress (config st)
          bgs          = Style bg bg
      Just Frame {..} -> ProgressBar . FancyS $
          [(spc2, defaultSty)
          ,(B $ spaces distance, fgs)
          ,(B $ spaces (width - distance), bgs)]
        where 
          width    = w - 4
          total    = curr + left
          distance = round ((curr / total) * fromIntegral width)
          curr     = realToFrac currentTime :: Float
          left     = realToFrac timeLeft
          (Style fg bg) = progress (config st)
          bgs           = Style bg bg
          fgs           = Style fg fg

------------------------------------------------------------------------

-- | Version info
instance Element PVersion where
    draw _ = PVersion $ P.pack versinfo

-- | Uptime
instance Element PTime where
    draw dd = PTime . uptime $ drawState dd

-- | Play mode
instance Element PMode where
    draw dd = PMode case status $ drawState dd of
                        Stopped -> "◼"
                        Paused  -> "Ⅱ"
                        Playing -> "▶"

-- | Loop, normal, or random
instance Element PMode2 where
    draw dd = PMode2 case mode $ drawState dd of
                        Random  -> "rand"
                        Loop    -> "loop"
                        Normal  -> "once"

------------------------------------------------------------------------

instance Element PlayModes where
    draw dd = PlayModes $ m ++ ' ' : m'
        where
            PMode  m  = draw dd
            PMode2 m' = draw dd

instance Element PlayInfo where
    draw dd = PlayInfo $ mconcat [
           -- TODO pregenerate as template
           spaces (P.length numd - P.length curd)
         , curd
         , "/", numd
         , " dir"
         , onPlural (snd . bounds $ folders st) "" "s"
         , " "
         , spaces (P.length numf - P.length curf)
         , curf
         , "/", numf
         , " file"
         , onPlural (size st) "" "s"
         ]
      where
        st = drawState dd
        tobs = P.pack . show
        onPlural 1 s _ = s
        onPlural _ _ p = p
        curf = tobs $ 1 + cursor st
        numf = tobs $ size st
        mydir = fdir $ music st ! cursor st
        curd = tobs $ 1 + mydir
        numd = tobs $ 1 + snd (bounds $ folders st)

instance Element PlayTitle where
    draw dd =
        PlayTitle $ FancyS $ map (,hl)
            if gap >= 2
            then [B $ mconcat [space,inf,spaces gapl], U modes,
                    B $ mconcat [spaces gapr,time,space,ver,space]]
            else let gap' = x - modlen; gapl' = gap' `div` 2
                 in if gap' >= 2
                    then [B $ spaces gapl', U modes, B $ spaces $ gap' - gapl']
                    else [B space, U $ take (x-2) modes, B space]
      where
        PlayInfo inf    = draw dd
        PTime time      = draw dd
        PlayModes modes = draw dd
        PVersion ver    = draw dd

        x       = sizeW $ drawSize dd
        lsize   = 1 + P.length inf
        rsize   = 2 + P.length time + P.length ver
        side    = (x - modlen) `div` 2
        gap     = x - modlen - lsize - rsize
        gapl    = 1 `max` ((side - lsize) `min` gap)
        gapr    = 1 `max` (gap - gapl)
        modlen  = 6 -- length modes
        space   = spaces 1
        hl      = titlebar . config $ drawState dd

-- | Playlist
instance Element PlayList where
    draw dd@DD{ drawSize=Size y x, drawPos=Pos{posY=o}, drawState=st } =
        PlayList $
            title
            : list
            ++ replicate (height - length list - 2) (Fast P.empty defaultSty)
            ++ [minibuffer st]
        where
            PlayTitle title = draw dd

            songs  = music st
            this   = current st
            curr   = cursor  st
            height = y - o

            -- number of screens down, and then offset
            buflen   = height - 2
            (screens,select) = quotRem curr buflen -- keep cursor in screen

            playing  = let top = screens * buflen
                           bot = (screens + 1) * buflen
                       in if this >= top && this < bot
                            then this - top -- playing song is visible
                            else (-1)

            -- visible slice of the playlist
            visible = slice off (off + buflen) songs
                where off = screens * buflen

            -- TODO rewrite as fold
            visible' :: [(Maybe Int, String)]
            visible' = loop (-1) visible where
                loop _ []     = []
                loop n (v:vs) =
                    let r = if fdir v > n then Just (fdir v) else Nothing
                    in (r, ellipsize (x - indent - 1) $ UTF8.toString $ fbase v)
                            : loop (fdir v) vs

            list   = [ drawIt . color $ n | n <- zip visible' [0..] ]

            indent = (round $ (0.334 :: Float) * fromIntegral x) :: Int
                
            color :: ((Maybe Int, String), Int)
                        -> (Maybe Int, Style, [AmbiString])
            color ((m,s),i) 
                | i == select && i == playing = f sty3
                | i == select                 = f sty2
                | i == playing                = f sty1
                | otherwise                   = (m, defaultSty, [U s])
                where
                    f sty = (m, sty, [
                        U s,
                        B $ spaces (x - indent - 1 - displayWidth s)])
            
            sty1 = selected . config $ st
            sty2 = cursors  . config $ st
            sty3 = combined . config $ st

            drawIt :: (Maybe Int, Style, [AmbiString]) -> StringA
            drawIt (Nothing, sty, v) =
                FancyS $ map (, sty) $ B (spaces (1 + indent)) : v

            drawIt (Just i, sty, v) = FancyS
                $ (U d, sty')
                : (B $ spaces (indent + 1 - displayWidth d), sty')
                : map (, sty) v
              where
                sty' = if sty == sty2 || sty == sty3 then sty2 else sty1
                d = ellipsize (indent - 1) $ UTF8.toString $ basenameP
                        $ case size st of
                            0 -> "(empty)"
                            _ -> dname $ folders st ! i

--
-- | Decode the list of current tracks
--
printPlayList :: PlayList -> [StringA]
printPlayList (PlayList s) = s
{-# INLINE printPlayList #-}
                
------------------------------------------------------------------------

-- | Calculate whitespaces, very common, so precompute likely values
spaces :: Int -> ByteString
spaces n
    | n <= 0    = ""
    | n > 100   = P.replicate n ' ' -- unlikely
    | otherwise = arr ! n
  where
    arr :: Array Int ByteString   -- precompute some whitespace strs
    arr = listArray (0,100) [ P.take i s100 | i <- [0..100] ]

    s100 :: ByteString
    s100 = P.replicate 100 ' '  -- seems reasonable

------------------------------------------------------------------------
--
-- | Now write out just the clock line
-- Speed things up a bit, just use read State.
--
redrawJustClock :: Draw
redrawJustClock = Draw $ discardErrors do
   st      <- getsST id
   let fr = clock st
   (h, w) <- screenSize
   let s = Size h w
   let (ProgressBar bar) = draw $ DD s undefined st fr :: ProgressBar
       (PTimes times)    = {-# SCC "redrawJustClock.times" #-}
                           draw $ DD s undefined st fr :: PTimes
   Curses.wMove Curses.stdScr 1 0   -- hardcoded!
   drawLine w bar
   Curses.wMove Curses.stdScr 2 0   -- hardcoded!
   drawLine w times
   -- drawHelp st fr s

------------------------------------------------------------------------
--
-- work for drawing help. draw the help screen if it is up
--
renderModal :: forall me. ModalElement me => HState -> Maybe Frame -> Size -> IO ()
renderModal st fr s@(Size h w) = do
   whenJust (drawModal @me $ DD s (Pos 0 0) st fr) \modal' -> do
       let Fast fps _ = head modal'
           offset     = max 0 $ (w - P.length fps) `div` 2
           height     = (h - length modal') `div` 2
       when (height > 0) do
            Curses.wMove Curses.stdScr ((h - length modal') `div` 2) offset
            mapM_ (\t -> do drawLine w t
                            (y',_) <- Curses.getYX Curses.stdScr
                            Curses.wMove Curses.stdScr (y'+1) offset) modal'

------------------------------------------------------------------------
--
-- | Draw the screen
--
redraw :: Draw
redraw = Draw $ discardErrors do
   -- linux ncurses, in particular, seems to complain a lot. this is an easy solution
   s <- getsST id    -- another refresh could be triggered?
   let f = clock s
   (h, w) <- screenSize
   let sz = Size h w

   let a = let x = printPlayScreen (draw $ DD sz (Pos 0 0) s f :: PlayScreen)
               y = printPlayList (draw $ DD sz (Pos (length x) 0) s f :: PlayList)
           in x ++ y

   when (xterm s) $ setXterm s
   
   gotoTop
   mapM_ (\t -> do drawLine w t
                   (y, x) <- Curses.getYX Curses.stdScr
                   fillLine
                   maybeLineDown t h y x )
         (take (h-1) (init a))
   renderModal @HelpScreen s f sz
   renderModal @HistScreen s f sz

   -- minibuffer
   Curses.wMove Curses.stdScr (h-1) 0
   fillLine 
   Curses.wMove Curses.stdScr (h-1) 0
   drawLine (w-1) (last a)
   when (miniFocused s) do -- a fake cursor
        drawLine 1 (Fast (spaces 1) (blockcursor . config $ s ))
        -- todo rendering bug here when deleting backwards in minibuffer

------------------------------------------------------------------------
--
-- | Draw a coloured (or not) string to the screen
--
drawLine :: Int -> StringA -> IO ()
drawLine _ (Fast ps sty) = drawAmbiString (B ps) sty
drawLine _ (FancyS ls) = traverse_ (uncurry drawAmbiString) ls

drawAmbiString :: AmbiString -> Style -> IO ()
drawAmbiString as sty = withStyle sty $ case as of
    B ps -> void $ B.useAsCString ps \cstr ->
                waddnstr Curses.stdScr cstr (fromIntegral $ P.length ps)
    U s  -> Curses.wAddStr Curses.stdScr s
{-# INLINE drawAmbiString #-}


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
    let (a,b) = bounds arr
    in [unsafeAt arr n | n <- [max a i .. min b j] ]
{-# INLINE slice #-}

-- isAscii :: ByteString -> Bool
-- isAscii = P.all (<'\128')

------------------------------------------------------------------------

--
-- | magics for setting xterm titles using ansi escape sequences
--
setXtermTitle :: [ByteString] -> IO ()
setXtermTitle strs = do
    mapM_ (P.hPut stderr) (before : strs ++ [after])
    hFlush stderr 
  where
    before = "\ESC]0;"
    after  = "\007"

------------------------------------------------------------------------

-- set xterm title (should have an instance Element)
-- Don't need to do this on each refresh...
setXterm :: HState -> IO ()
setXterm s = setXtermTitle $ case status s of
    Playing -> case id3 s of
        Nothing -> case size s of
                        0 -> ["hmp3"]
                        _ -> [fbase $ music s ! current s]
        Just ti -> id3artist ti :
                   if P.null (id3title ti)
                        then []
                        else [": ", id3title ti]
    Paused  -> ["paused"]
    Stopped -> ["stopped"]

displayWidth :: String -> Int
displayWidth = sum . map charWidth

ellipsize :: Int -> String -> String
ellipsize w s
  | displayWidth s <= w = s
  | True = go 0 0 s where
    go !i !l (c:s') =
        if l' > w-1
            then take i s ++ replicate (w-l) '…'
            else go (i+1) l' s'
      where l' = l + charWidth c
    go _  _ _ = error "Should've been in first case!"

charWidth :: Char -> Int
charWidth = fromIntegral . wcwidth . toEnum . fromEnum

foreign import ccall safe
    wcwidth :: CWchar -> CInt

--  Not exported by hscurses.
foreign import ccall safe
    waddnstr :: Curses.Window -> CString -> CInt -> IO CInt

