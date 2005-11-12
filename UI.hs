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
import Config
import Syntax hiding (draw)
import Utils

import Text.Printf

import Data.IORef
import System.IO

import qualified Curses

import qualified Control.Exception

import System.Posix.Signals         ( raiseSignal, sigTSTP )

--
-- | how to initialise the ui
--
start :: IO ()
start = do
    Curses.initCurses (resizeui >> redraw >> refresh >> return ())          -- initialise the screen
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
        ls  = [warnings sty, window sty, selected sty, highlight sty, progress sty]
        (Style fg bg) = progress sty -- an extra style
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
               ptitle :: !PTitle
              ,ptrack :: !PPlaying
              ,pbar   :: !ProgressBar
              ,ptime  :: !PTimes
        }

newtype PlayList = PlayList [StringA]

newtype PPlaying    = PPlaying    StringA
newtype PTitle      = PTitle      StringA
newtype PTrack      = PTrack      StringA
newtype PVersion    = PVersion    StringA
newtype PMode       = PMode       StringA
newtype ProgressBar = ProgressBar StringA
newtype PTimes      = PTimes      StringA
newtype PInfo       = PInfo       StringA

------------------------------------------------------------------------

instance Element PlayScreen where
    draw w x y z = PlayScreen a b c d
        where
            a = draw w x y z :: PTitle
            b = draw w x y z :: PPlaying
            c = draw w x y z :: ProgressBar
            d = draw w x y z :: PTimes

--
-- | Decode the play screen
--
printPlayScreen :: PlayScreen -> [StringA]
printPlayScreen (PlayScreen (PTitle a) 
                            (PPlaying b) 
                            (ProgressBar c) 
                            (PTimes d)) = [a , [] , b , c , d , []]

------------------------------------------------------------------------

instance (Element a, Element b) => Element (a,b) where
    draw a b c d = (draw a b c d, draw a b c d)

-- | Title of playing element widget
instance Element PTitle where
    draw p@(_,w) x y z = PTitle $ 
        color " " ++ mod ++ color (replicate gap ' ') ++ ver ++ color " "

        where (PMode mod)    = draw p x y z :: PMode
              (PVersion ver) = draw p x y z :: PVersion
              gap     = w - padding - length mod - length ver
              hl      = highlight (style config)
              color   = map (\c -> A c hl)
              padding = 2

-- | Version info
instance Element PVersion where
    draw _ _ _ _ = PVersion . map (\c -> A c (highlight . style $ config)) $ versinfo

-- | Play mode
instance Element PMode where
    draw _ _ st _ = PMode $ map (\c -> A c (highlight . style $ config)) $ 
        case status st of
            Stopped -> "[]"
            Paused  -> "||"
            Playing -> ">>"

instance Element PPlaying where
    draw w@(_,x') x y z = PPlaying $ C ' ' : C ' ' : (alignLR (x'-4) a b) ++ [C ' ' , C ' ']
        where
            (PTrack a) = draw w x y z :: PTrack
            (PInfo b)  = draw w x y z :: PInfo

-- | Play mode
instance Element PTrack where
    draw (y,_) _ st _ = 
        PTrack $ map C $ basename $ (music st) !! (current st)

-- | mp3 information
instance Element PInfo where
    draw _ _ st mfr = PInfo $ map C $ case info st of
        Nothing  -> []
        Just i   -> concat ["mpeg " ,(clean . show . version $ i)  ," "
                           ,"layer " ,(show . layer $ i) ," "
                           ,(show . bitrate $ i) ,"kbit/s "
                           ,(show ((sampleRate i) `div` 1000) ) ,"kHz"
                            {-playMode i-} ]

-- | The time used and time left
instance Element PTimes where
    draw _ _ _ Nothing       = PTimes []
    draw (y,x) _ _ (Just fr) = PTimes $ emptyA <++> elapsed ++ gap ++ remaining
      where
        elapsed   = map C ((printf  "%01d:%02d" lm lm') :: String)
        remaining = map C ((printf "-%01d:%02d" rm rm') :: String)
        gap       = replicate distance (C ' ')
        distance  = x - 4{-2 on each end-} - length elapsed - length remaining
        (lm,lm')  = quotRem (fst $ currentTime fr) 60
        (rm,rm')  = quotRem (fst $ timeLeft fr) 60

-- | A progress bar
instance Element ProgressBar where
    draw (_,w) _ _ Nothing = ProgressBar $ emptyA <++> replicate (w-4) (space bgs)
        where 
          (Style _ bg) = progress (style config)
          bgs          = Style bg bg

    draw (_,w) _ _ (Just fr) = ProgressBar $
            emptyA <++> 
            replicate distance (space fgs) ++
            replicate (width - distance) (space bgs)
        where 
          width    = w - 4
          total    = curr + left
          distance = round ((curr / total) * fromIntegral width)
          curr     = toFloat (currentTime fr)
          left     = toFloat (timeLeft fr)
          (Style fg bg) = progress (style config)
          bgs           = Style bg bg
          fgs           = Style fg fg
          toFloat (x,y) = (fromIntegral x :: Float) + (fromIntegral y / 100)

-- | Playlist, TODO this should do threading-style rendering of filesystem trees
-- TODO highlight selected entry. Scroll.
instance Element PlayList where
    draw (y,x) (o,_) st _ = 
        PlayList $ title 
                 : list 
                 ++ (replicate (height - length list - 2) [])
                 ++ [minibuffer st]
        where
            title  =  space hl
                   :  (setOn highlight . show . length $ list)
                   ++ (setOn highlight (" file" ++ if length list == 1 then [] else "s"))
                   ++ replicate x (A ' ' hl)

            hl     = highlight (style config)
            songs  = music st
            this   = current st
            height = y - o
            list   = [ uncurry color m
                     | m <- zip (map basename (take y songs)) [0..] ]

            color s i | i == this = setOn selected $ s ++ 
                                        replicate (x - length s) ' '
                      | otherwise = map C s

            setOn f = map (\c -> A c (f (style config)))

--
-- | Decode the list of current tracks
--
printPlayList :: PlayList -> [StringA]
printPlayList (PlayList s) = s
                
------------------------------------------------------------------------

-- add some space
(<++>) :: StringA -> StringA -> StringA
x <++> y = x ++ [C ' ', C ' '] ++ y

emptyA :: StringA
emptyA = []

space :: Style -> CharA
space = A ' ' 

------------------------------------------------------------------------

-- | Take two strings, and pad them in the middle
alignLR :: Int -> StringA -> StringA -> StringA
alignLR w l r = l ++ (replicate (w-length l - length r) (C ' ')) ++ r 

-- | Pad a string on the left
alignR :: Int -> StringA -> StringA
alignR  w t   = (replicate (w-5-length t) (C ' ')) ++ t

-- | Pad a string on the right
alignL :: Int -> StringA -> StringA
alignL w t = t ++ (replicate (w - 5 - length t) (C ' '))

-- | Single char padding 
padL, padR, pad :: StringA -> StringA
padL s = C ' ' : s
padR s = s ++ [C ' ']
pad = padR . padL

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
   Curses.wMove Curses.stdScr 3 0   -- hardcoded!
   drawLine w bar
   Curses.wMove Curses.stdScr 4 0   -- hardcoded!
   drawLine w times

------------------------------------------------------------------------

--
-- | Draw the screen
--
redraw :: IO ()
redraw = withState $ \s -> do
   sz@(h,w) <- screenSize
   f <- readClock id
   let x = printPlayScreen (draw sz (0,0)        s f :: PlayScreen)
       y = printPlayList   (draw sz (length x,0) s f :: PlayList)
       a = x ++ y -- all lines
   gotoTop
   mapM_ (drawLine w) (take (h-1) (init a))
   Curses.wMove Curses.stdScr h 0
   drawLine (w-1) (last a)

lineDown :: Int -> IO ()
lineDown h = do
    (y,_) <- Curses.getYX Curses.stdScr
    Curses.wMove Curses.stdScr (y+1) 0

--
-- | lazy version is faster than calculating length of s
-- Optimise this?
--
drawLine :: Int -> StringA -> IO ()
drawLine w s = flip mapM_ (take w (s ++ repeat (C ' '))) $ \ac -> case ac of
                C c     -> Curses.wAddChar Curses.stdScr c
                A c sty -> withStyle sty (Curses.wAddChar Curses.stdScr c)

--
-- | Fill to end of line spaces
--
-- fillLine :: IO ()
-- fillLine = Curses.clrToEol

--
-- | move cursor to origin of stdScr.
--
gotoTop :: IO ()
gotoTop = Curses.wMove Curses.stdScr 0 0

