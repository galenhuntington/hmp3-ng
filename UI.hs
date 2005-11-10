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
import Syntax
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
    Curses.initCurses (resizeui >> return ())          -- initialise the screen
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
        ls  = [window sty, highlight sty, progress sty]
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
              resizeui >> return ()
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
--
-- | Render the state to a list of strings representing the state
--
-- should have a pure state manipulation, really
--
renderSt :: IO [StringA]
renderSt = do
    (h,w)    <- screenSize
    s        <- readSt id
    fr       <- readClock id
    let songs = music s
        i     = current s
        track = songs !! i
        inf   = info s
        stat  = status s

        hl    = highlight (style config)
        pr    = progress (style config)

        hline = replicate w (A ' ' hl)

        playtime = renderClock w fr
        prog     = renderProgress pr w fr

        title    = let colorize = map (\c -> A c hl)
                       l = show stat
                       r = versinfo
                   in colorize $ ' ' : l 
                               ++ (replicate (w-3-length l - length r) ' ') 
                               ++ r  ++ " "
        
        playing :: [StringA]
        playing = title
                : []
                : pad (alignL (w-2) (map C $ basename track))
                : pad details
                : []
                : prog
                : playtime 
                : [] 

        details = map C $ case inf of   -- todo use combinators
                Nothing  -> "-"
                Just inf' ->
                     "(MPEG-" ++ clean (show (version inf'))  ++ " "
                  ++ "Layer " ++ show (layer inf')      ++ " "
                  ++ show (bitrate inf') ++ "kbit/s"   ++ " "
                  ++ show (sampleRate inf') ++ "Hz"     ++  " "
                  ++ (playMode inf') ++ ")"

        playlist = let y  = h - (length playing - 2)
                       ss = map basename (take (y - 6) songs)
                   in map (pad . map C) $ ss ++ (replicate (y - length ss - 6) [])

    return $ playing ++ [hline] ++ playlist

------------------------------------------------------------------------
--
-- | Render the clock line
--
renderClock :: Int -> Maybe Frame -> StringA
renderClock w Nothing = []
renderClock w (Just fr) = 
   let (l,_)    = currentTime fr
       (r,_)    = timeLeft fr
       (lm,lm') = quotRem l 60
       (rm,rm') = quotRem r 60
   in C ' ': C ' ' : C ' ' : 
                alignLR (w-1) (map C ((printf  "%01d:%02d" lm lm') :: String)) 
                              (map C ((printf "-%01d:%02d" rm rm') :: String))

--
-- | Render the progress bar
--
renderProgress :: Style -> Int -> (Maybe Frame) -> StringA
renderProgress (Style _  bg)  w Nothing  = 
      C ' ' : C ' ' : C ' ' : replicate (w-6) (A ' ' (Style bg bg))

renderProgress (Style fg bg) w (Just fr) = 
   let a        = toFloat (currentTime fr)
       b        = toFloat (timeLeft fr)
       total    = a + b
       width    = w - 6
       distance = round ((a / total) * fromIntegral width)
   in C ' ' : C ' ' : C ' ' : replicate distance           (A ' ' (Style fg fg)) ++
                              replicate (width - distance) (A ' ' (Style bg bg))

   where
       toFloat (x,y) = (fromIntegral x :: Float) + (fromIntegral y / 100)

------------------------------------------------------------------------

-- | Take two strings, and pad them in the middle
alignLR :: Int -> StringA -> StringA -> StringA
alignLR w l r = l ++ (replicate (w-5-length l - length r) (C ' ')) ++ r 

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
redrawJustClock = do
   (_,w) <- screenSize
   fr    <- readClock id
   let pr    = progress (style config)
   Curses.wMove Curses.stdScr 5 0
   drawLine (w-1) (renderProgress pr w fr)    -- draw the time / remaining line

   Curses.wMove Curses.stdScr 6 0
   drawLine (w-1) (renderClock w fr)    -- draw the time / remaining line

------------------------------------------------------------------------

--
-- | Draw the screen
--
redraw :: IO ()
redraw = do
   ls    <- renderSt
   (_,w) <- screenSize
   gotoTop
   mapM_ (\s -> drawLine (w-1) s >> lineDown) (init ls)
   drawLine (w-1) (last ls)
   gotoTop

lineDown :: IO ()
lineDown = do
    (h,_) <- screenSize
    (y,_) <- Curses.getYX Curses.stdScr
    Curses.wMove Curses.stdScr (min h (y+1)) 0

--
-- | lazy version is faster than calculating length of s
--
drawLine :: Int -> StringA -> IO ()
drawLine w s = flip mapM_ (take w (s ++ repeat (C ' '))) $ \c -> case c of
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

