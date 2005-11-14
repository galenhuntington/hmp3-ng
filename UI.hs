{-# OPTIONS -cpp -#include "curses.h" #-}

#if HAVE_SIGNAL_H
{-#include <signal.h> #-}
#endif

#include "config.h"

-- 
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

        -- * Construction, destruction
        start, end, suspend, screenSize, refresh, refreshClock,

        -- * Input
        getKey

  )   where

import Style
import State
import Syntax hiding (draw)
import Utils
import Config
import qualified Curses

import {-# SOURCE #-} Keymap

import Control.Monad
import Data.IORef
import Data.List
import Data.Char
import System.IO
import Text.Printf
import qualified Control.Exception

import System.Posix.Signals         ( raiseSignal, sigTSTP )
import System.Posix.Env

import Foreign.Ptr
import Foreign.Storable

import qualified Data.FastPackedString as P

import Debug.Trace

--
-- | how to initialise the ui
--
start :: IO ()
start = do
    Control.Exception.handle (const $ return ()) $ do -- tweak for OpenBSD console
        term <- getEnv "TERM"
        hPutStrLn stderr (show term)
        case term of Just "vt220" -> putEnv "TERM=xterm-color"
                     _            -> return ()

    Curses.initCurses refresh
    initcolours
    Curses.keypad Curses.stdScr True    -- grab the keyboard
    Control.Exception.catch (Curses.cursSet (fromIntegral (0::Int)) >> return ()) 
                            (\_ -> return ())

--
-- | And turn on the colours
--
initcolours :: IO ()
initcolours = do

    let sty = style config
        ls  = [helpscreen sty, warnings sty, window sty, 
               selected sty, highlight sty, progress sty,
               cursors sty, combined sty ]
        (Style fg bg) = progress sty    -- bonus style
        
    pairs <- initUiColors (ls ++ [Style bg bg, Style fg fg])
    writeIORef pairMap pairs
    uiAttr (window (style config)) >>= \(_,p) -> Curses.bkgrndSet nullA p

--
-- | Clean up and go home. Refresh is needed on linux. grr.
--
end :: IO ()
end = Curses.endWin

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
-- We only need to refresh if we don't have the SIGWINCH signal handler
-- working for us.
--
getKey :: IO Char
getKey = do
    k <- Curses.getCh
    if k == Curses.keyResize 
        then do
#ifndef SIGWINCH
              redraw >> resizeui >> return ()   -- XXX ^L doesn't work
#endif
              getKey
        else return k
 
-- | Resize the window
-- From "Writing Programs with NCURSES", by Eric S. Raymond and Zeyd M. Ben-Halim
--
resizeui :: IO (Int,Int)
resizeui = do
    Curses.endWin
    Curses.resetParams
    Curses.refresh
    Curses.scrSize

refresh :: IO ()
refresh = redraw >> Curses.refresh

refreshClock :: IO ()
refreshClock = redrawJustClock >> Curses.refresh

------------------------------------------------------------------------

type Pos    = (Int{-H-}, Int{-W-})
type Size   = (Int{-H-}, Int{-W-})

--
-- | A class for renderable objects, given the application state, and
-- for printing the object as a list of strings
--
class Element a where
    draw  :: Size -> Pos -> State -> Maybe Frame -> a

--
-- | The elements of the play mode widget
--
data PlayScreen = 
        PlayScreen {
               ptrack :: !PPlaying
              ,pbar   :: !ProgressBar
              ,ptime  :: !PTimes
        }

newtype PlayList = PlayList [StringA]

newtype PPlaying    = PPlaying    StringA
newtype PVersion    = PVersion    StringA
newtype PMode       = PMode       StringA
newtype ProgressBar = ProgressBar StringA
newtype PTimes      = PTimes      StringA

newtype PTrack      = PTrack      P.FastString
newtype PInfo       = PInfo       String

------------------------------------------------------------------------

instance Element PlayScreen where
    draw w x y z = PlayScreen a b c
        where
            a = draw w x y z :: PPlaying
            b = draw w x y z :: ProgressBar
            c = draw w x y z :: PTimes

--
-- | Decode the play screen
--
printPlayScreen :: PlayScreen -> [StringA]
printPlayScreen (PlayScreen (PPlaying a) 
                            (ProgressBar b) 
                            (PTimes c)) = [a , b , c]

------------------------------------------------------------------------

instance (Element a, Element b) => Element (a,b) where
    draw a b c d = (draw a b c d, draw a b c d)

------------------------------------------------------------------------

instance Element PPlaying where
    draw w@(_,x') x y z = PPlaying $ 
            Fast (pad `P.append` alignLR (x'-4) a (P.pack b)) sty
        where
            pad = P.packAddress "  "#
            sty = Style Default Default
            (PTrack a) = draw w x y z :: PTrack
            (PInfo b)  = draw w x y z :: PInfo

-- | Play mode
instance Element PTrack where
    draw (y,_) _ st _ = PTrack . snd $ (music st) !! (current st)

-- | mp3 information
instance Element PInfo where
    draw _ _ st mfr = PInfo $ case info st of
        Nothing  -> []
        Just i   -> concat ["mpeg " ,(clean . show . version $ i)  ," "
                           ,"layer " ,(show . layer $ i) ," "
                           ,(show . bitrate $ i) ,"kbit/s "
                           ,(show ((sampleRate i) `div` 1000) ) ,"kHz"
                            {-playMode i-} ]

------------------------------------------------------------------------

newtype HelpScreen = HelpScreen [StringA]

instance Element HelpScreen where
    draw (_,w) _ _ _ = HelpScreen $ [ Fast (f cs h) sty | (h,cs,_) <- keyTable ]
        where
            sty = helpscreen . style $ config 

            f :: [Char] -> P.FastString -> P.FastString
            f cs ps = 
                let p = P.pack str `P.append` ps
                    s = P.pack (take (tot - P.length p) (repeat ' '))
                in p `P.append` s
                where
                    tot = round (fromIntegral w * 0.7)
                    len = round (fromIntegral tot * 0.4)
                    str = take len $ ' ' :
                            (concat . intersperse " " $ (map ppr cs)) ++ repeat ' '

                    ppr c = case c of
                        k | k == Curses.keyUp    -> "Up"
                          | k == Curses.keyDown  -> "Down"
                          | k == Curses.keyPPage -> "PgUp"
                          | k == Curses.keyNPage -> "PgDn"
                          | k == Curses.keyLeft  -> "Left"
                          | k == Curses.keyRight -> "Right"
                        _ -> show c
                        
                            

------------------------------------------------------------------------

-- | The time used and time left
instance Element PTimes where
    draw _ _ _ Nothing       = PTimes . Plain $ []
    draw (y,x) _ _ (Just fr) = PTimes $ Plain $! "  " ++elapsed++gap++remaining
      where
        elapsed   = (printf  "%01d:%02d" lm lm') :: String
        remaining = (printf "-%01d:%02d" rm rm') :: String
        gap       = replicate distance ' '
        distance  = x - 4{-2 on each end-} - length elapsed - length remaining
        (lm,lm')  = quotRem (fst $ currentTime fr) 60
        (rm,rm')  = quotRem (fst $ timeLeft fr) 60

------------------------------------------------------------------------

-- | A progress bar
instance Element ProgressBar where
    draw (_,w) _ _ Nothing = ProgressBar . Fancy $
          A ' ' df : A ' ' df : replicate (w-4) (A ' ' bgs)

        where 
          (Style _ bg) = progress (style config)
          df           = Style Default Default
          bgs          = Style bg bg

    draw (_,w) _ _ (Just fr) = ProgressBar . Fancy $
          A ' ' df : A ' ' df :
          replicate distance (A ' ' fgs) ++
          replicate (width - distance) (A ' ' bgs)

        where 
          df           = Style Default Default
          width    = w - 4
          total    = curr + left
          distance = round ((curr / total) * fromIntegral width)
          curr     = toFloat (currentTime fr)
          left     = toFloat (timeLeft fr)
          (Style fg bg) = progress (style config)
          bgs           = Style bg bg
          fgs           = Style fg fg
          toFloat (x,y) = (fromIntegral x :: Float) + (fromIntegral y / 100)

------------------------------------------------------------------------

-- | Version info
instance Element PVersion where
    draw _ _ _ _ = PVersion . Fancy . map (\c -> A c (highlight . style $ config)) $ versinfo

-- | Play mode
instance Element PMode where
    draw _ _ st _ = PMode . Fancy . map (\c -> A c (highlight . style $ config)) $ 
        case status st of
            Stopped -> "[]"
            Paused  -> "||"
            Playing -> ">>"

------------------------------------------------------------------------

-- | Playlist, TODO this should do threading-style rendering of filesystem trees
-- TODO highlight selected entry. Scroll.
-- Should simplify this, partitioning on newtyped elements
instance Element PlayList where
    draw p@(y,x) q@(o,_) st z =
        PlayList $! title 
                 : list 
                 ++ (replicate (height - length list - 2) (Plain []))
                 ++ [minibuffer st]
        where
            (PMode mod@(Fancy m))    = draw p q st z :: PMode
            (PVersion ver@(Fancy v)) = draw p q st z :: PVersion
            inf = percent ++ " (" ++ (show . length $ songs) ++
                          " file" ++ (if length songs == 1 then [] else "s") ++ ")"

            percent | percent' == 0  && curr == 0 = "Top"
                    | percent' == 100 = "All"
                    | otherwise       = show percent' ++ "%"
  
            percent' :: Int= round $ ((fromIntegral curr) / 
                                     ((fromIntegral . length $ songs) - 1) * 100.0 :: Float)
            padding        = 2

            gap            = x - padding - length inf - length m - length v
            gapl           = gap `div` 2
            gapr           = gap - gapl

            title  = Fancy [space hl]
                  >< setOn highlight inf
                  >< Fancy (replicate gapl (A ' ' hl))
                  >< mod
                  >< Fancy (replicate gapr (A ' ' hl))
                  >< ver
                  >< Fancy [space hl]

            hl     = highlight (style config)
            songs  = music st
            this   = current st
            curr   = cursor  st
            height = y - o

            -- number of screens down, and then offset
            buflen    = height - 2
            (screens,select) = quotRem curr buflen -- keep cursor in screen

            playing  = let top = screens * buflen
                           bot = (screens + 1) * buflen
                       in if this >= top && this < bot
                            then this - top -- playing song is visible
                            else (-1)

            visible   = drop (screens*buflen) songs -- take the visible songs
    
            -- no scrolling:
            list   = [ uncurry color m
                     | m <- zip (map snd visible) [0..] ]

            color s i 
                | i == select && i == playing
                = Fast (s `P.append` P.pack (replicate (x - P.length s) ' ')) sty3

                | i == select
                = Fast (s `P.append` P.pack (replicate (x - P.length s) ' ')) sty2

                | i == playing
                = Fast (s `P.append` P.pack (replicate (x - P.length s) ' ')) sty1

                | otherwise = Fast s (Style Default Default)
                where
                    sty1 = selected . style $ config
                    sty2 = cursors  . style  $ config
                    sty3 = combined . style $ config

            setOn f = Fancy . map (\c -> A c (f (style config)))

--
-- | Decode the list of current tracks
--
printPlayList :: PlayList -> [StringA]
printPlayList (PlayList s) = s
                
------------------------------------------------------------------------

space :: Style -> CharA
space = A ' ' 

-- | Take two strings, and pad them in the middle
alignLR :: Int -> P.FastString -> P.FastString -> P.FastString
alignLR w l r | padding >= 0 = l `P.append` gap `P.append` r 
              | otherwise    = P.take (w - P.length r - 4) l `P.append` ellipsis `P.append` r
    where padding = w - P.length l - P.length r
          gap     = P.pack $ replicate padding ' '
          ellipsis= P.packAddress "... "#

------------------------------------------------------------------------
--
-- | Now write out just the clock line
--
redrawJustClock :: IO ()
redrawJustClock = withState $ \st -> do
   fr      <- readClock id
   s@(_,w) <- screenSize
   let (ProgressBar bar) = draw s undefined st fr :: ProgressBar
       (PTimes times)    = draw s undefined st fr :: PTimes
   Curses.wMove Curses.stdScr 1 0   -- hardcoded!
   drawLine w bar
   Curses.wMove Curses.stdScr 2 0   -- hardcoded!
   drawLine w times
   return ()

------------------------------------------------------------------------

--
-- | Draw the screen
--
redraw :: IO ()
redraw = withState $ \s -> do

   sz@(h,w) <- screenSize
   f <- readClock id
   let x = {-# SCC "redraw.playscreen" #-} printPlayScreen (draw sz (0,0) s f :: PlayScreen)
       y = {-# SCC "redraw.playlist" #-} printPlayList   (draw sz (length x,0) s f :: PlayList)
       a = x ++ y
   gotoTop
   {-# SCC "redraw.draw" #-}mapM_ (\s -> do drawLine w s 
                                            fillLine
                                            (y,x) <- Curses.getYX Curses.stdScr
                                            maybeLineDown s h y x )
         (take (h-1) (init a))

   Curses.wMove Curses.stdScr (h-1) 0
   drawLine (w-1) (last a) >> fillLine

   when (helpVisible s) $ do
       let (HelpScreen help) = {-# SCC "redraw.help" #-} draw sz (0,0) s f :: HelpScreen
           (Fast fps _) = head help
           offset = (w - (P.length fps)) `div` 2
       Curses.wMove Curses.stdScr ((h - length help) `div` 2) offset
       mapM_ (\s -> do drawLine w s
                       (y,_) <- Curses.getYX Curses.stdScr
                       Curses.wMove Curses.stdScr (y+1) offset) help

------------------------------------------------------------------------
--
-- | Draw a coloured (or not) string to the screen
--
drawLine :: Int -> StringA -> IO ()

drawLine w (Fancy s) =
    flip mapM_ s $ \fs -> case fs of
        C c     -> Curses.wAddChar Curses.stdScr c
        A c sty -> withStyle sty $ Curses.wAddChar Curses.stdScr c

drawLine _ (Plain s) = Curses.wAddStr Curses.stdScr s

drawLine _ (Fast ps sty) = withStyle sty $ P.unsafeUseAsCString ps $ \cstr -> 
    Curses.throwIfErr_ "drawLine"# $
        Curses.waddnstr Curses.stdScr cstr (fromIntegral . P.length $ ps)

------------------------------------------------------------------------

maybeLineDown (Plain []) h y _ = lineDown h y
maybeLineDown (Fancy []) h y _ = lineDown h y
maybeLineDown _ h y x
    | x == 0    = return ()     -- already moved down
    | otherwise = lineDown h y

------------------------------------------------------------------------

lineDown :: Int -> Int -> IO ()
lineDown h y = Curses.wMove Curses.stdScr (min h (y+1)) 0

--
-- | Fill to end of line spaces
--
fillLine :: IO ()
fillLine = Curses.clrToEol

--
-- | move cursor to origin of stdScr.
--
gotoTop :: IO ()
gotoTop = Curses.wMove Curses.stdScr 0 0

