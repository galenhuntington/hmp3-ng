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
    pairs <- initUiColors (style config)
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
renderSt :: IO [String]
renderSt = do
    (h,w) <- screenSize
    s  <- readSt id
    fr <- readClock id
    let songs = music s
        i     = current s
        track = songs !! i
        inf   = info s
        stat  = status s

        hline    = '+' : (replicate (w - 3) '-') ++ ['+']
        box ss   = hline : (map border ss) ++ [hline]
        border t = "| "++ t ++ (replicate (w - 5 - length t) ' ') ++" |"

        alignLR l r = l ++ (replicate (w-5-length l - length r) ' ') ++ r 
        alignR  t   = (replicate (w-5-length t) ' ') ++ t
        
        playing :: [String]
        playing = alignLR (basename track) (show stat) 
                : details
                : []
                : []
                : playtime : []

        details = case inf of   -- todo use combinators
                Nothing  -> "-"
                Just inf' ->
                     "(MPEG-" ++ clean (show (version inf'))  ++ " "
                  ++ "Layer " ++ show (layer inf')      ++ " "
                  ++ show (bitrate inf') ++ "kbit/s"   ++ " "
                  ++ show (sampleRate inf') ++ "Hz"     ++  " "
                  ++ (playMode inf') ++ ")"

        playtime = case fr of
                Nothing -> "-"
                Just fr' -> 
                   let (l,_) = currentTime fr'
                       (r,_) = timeLeft fr'
                       (lm,lm') = quotRem l 60
                       (rm,rm') = quotRem r 60
                       
                       str = (printf "%01d:%02d / %01d:%02d" lm lm' rm rm') :: String

                   in alignR str

        playlist = 
                let y  = h - (length playing - 2)
                    ss = map basename (take (y - 6) songs)
                in ss ++ (replicate (y - length ss - 6) [])

    return $ (box playing) ++ (box playlist)

redrawJustClock :: IO ()
redrawJustClock = do
   strs  <- renderSt
   (_,w) <- screenSize
   Curses.wMove Curses.stdScr 5 0
   drawLine (w-1) (strs !! 5)

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
drawLine :: Int -> String -> IO ()
drawLine w s = Curses.wAddStr Curses.stdScr $! take w (s ++ repeat ' ')

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

