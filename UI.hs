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
        start, end, suspend, screenSize, refresh, refreshClock, resetui,
        setXtermTitle,

        -- * Input
        getKey

  )   where

import Style
import FastIO
import Tree
import State
import Syntax hiding (draw)
import Config
import qualified Curses

import {-# SOURCE #-} Keymap

import Data.IORef
import Data.List
import Data.Maybe
import Data.Char
import Data.Array
import Data.Array.Base  ( unsafeAt )

import System.IO

import Text.Printf

import Control.Monad
import qualified Control.Exception
import Foreign.C.String

import System.Posix.Signals         ( raiseSignal, sigTSTP )
import System.Posix.Env

import qualified Data.FastPackedString as P

--
-- | how to initialise the ui
--
start :: IO ()
start = do
    Control.Exception.handle (const $ return ()) $ do -- tweak for OpenBSD console
        thisterm <- getEnv "TERM"
        case thisterm of 
            Just "vt220" -> putEnv "TERM=xterm-color"
            Just t | "xterm" `isPrefixOf` t 
                   -> modifyState_ $ \st -> return st { xterm = True }
            _ -> hPutStrLn stderr "not in an xterm"

    Curses.initCurses resetui

    -- working out if we can do colours
    b <- Curses.hasColors
    initcolours $ if b then style config else {- do something better -} style config

    Curses.keypad Curses.stdScr True    -- grab the keyboard
    nocursor

-- | Rezet
resetui :: IO ()
resetui = resizeui >> nocursor >> refresh

-- | And force invisible
nocursor :: IO ()
nocursor = do
    Control.Exception.catch (Curses.cursSet (fromIntegral (0::Int)) >> return ()) 
                            (\_ -> return ())

--
-- | And turn on the colours
--
initcolours :: UIStyle -> IO ()
initcolours sty = do
    let ls  = [helpscreen sty, warnings sty, window sty, 
               selected sty, highlight sty, progress sty,
               cursors sty, combined sty ]
        (Style fg bg) = progress sty    -- bonus style
        
    pairs <- initUiColors (ls ++ [Style bg bg, Style fg fg])
    writeIORef pairMap pairs
    uiAttr (window (style config)) >>= \(_,p) -> Curses.bkgrndSet nullA p

--
-- | Clean up and go home. Refresh is needed on linux. grr.
--
end :: Bool -> IO ()
end isXterm = do Curses.endWin
                 when isXterm $ setXtermTitle (P.packAddress "xterm"#)

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
               _ptrack :: !PPlaying
              ,_pbar   :: !ProgressBar
              ,_ptime  :: !PTimes
        }

-- How does this all work? Firstly, we mostly want to draw fast strings
-- directly to the screen. To break the drawing problem down, you need
-- to write an instance of Element for each element in the ui. Larger
-- and larger elements then combine these items together. 
--
-- Obviously to write the element instance, you need a new type for each
-- element, to disinguish them. As follows:

newtype PlayList = PlayList [StringA]

newtype PPlaying    = PPlaying    StringA
newtype PVersion    = PVersion    P.FastString
newtype PMode       = PMode       P.FastString
newtype PMode2      = PMode2      P.FastString
newtype ProgressBar = ProgressBar StringA
newtype PTimes      = PTimes      StringA

newtype PInfo       = PInfo       P.FastString
newtype PId3        = PId3        P.FastString
newtype PTime       = PTime       P.FastString

newtype PlayTitle = PlayTitle StringA
newtype PlayInfo  = PlayInfo  P.FastString
newtype PlayModes = PlayModes P.FastString
newtype HelpScreen = HelpScreen [StringA]

------------------------------------------------------------------------

instance Element PlayScreen where
    draw w x y z = PlayScreen a b c
        where
            a = draw w x y z :: PPlaying
            b = draw w x y z :: ProgressBar
            c = draw w x y z :: PTimes

-- | Decode the play screen
printPlayScreen :: PlayScreen -> [StringA]
printPlayScreen (PlayScreen (PPlaying a) 
                            (ProgressBar b) 
                            (PTimes c)) = [a , b , c]

------------------------------------------------------------------------

instance (Element a, Element b) => Element (a,b) where
    draw a b c d = (draw a b c d, draw a b c d)

------------------------------------------------------------------------

-- Info about the current track
instance Element PPlaying where
    draw w@(_,x') x st z = PPlaying $ 
            Fast (pad `P.append` alignLR (x'-4) a b) sty
        where
            pad = P.packAddress "  "#
            sty = Style Default Default

            (PId3 a)  = draw w x st z :: PId3 
            (PInfo b) = draw w x st z :: PInfo

-- | Id3 Info
instance Element PId3 where
    draw _ _ st _ = case id3 st of
        Nothing -> PId3 . fbase $! (music st) ! (current st)
        Just i  -> PId3 $! id3str i

-- | mp3 information
instance Element PInfo where
    draw _ _ st _ = PInfo $ case info st of
        Nothing  -> P.empty
        Just i   -> userinfo i

------------------------------------------------------------------------

instance Element HelpScreen where
    draw (_,w) _ _ _ = HelpScreen $ 
        [ Fast (f cs h) sty | (h,cs,_) <- keyTable ] ++
        [ Fast (f cs h) sty | (h,cs) <- extraTable ]
        where
            sty  = helpscreen . style $ config 

            f :: [Char] -> P.FastString -> P.FastString
            f cs ps = 
                let p = P.pack str `P.append` ps
                    s = P.pack (take (tot - P.length p) (repeat ' '))
                in p `P.append` s
                where
                    tot = round (fromIntegral w * (0.8::Float))
                    len = round (fromIntegral tot * (0.2::Float))

                    -- faststringify
                    str = take len $ ' ' :
                            (concat . intersperse " " $ (map pprIt cs)) ++ repeat ' '

                    pprIt c = case c of
                        k | k == Curses.keyUp    -> "Up"
                          | k == Curses.keyDown  -> "Down"
                          | k == Curses.keyPPage -> "PgUp"
                          | k == Curses.keyNPage -> "PgDn"
                          | k == Curses.keyLeft  -> "Left"
                          | k == Curses.keyRight -> "Right"
                          | k == '\n'            -> "Enter"
                          | k == '\f'            -> "^L"
                          | k == Curses.keyEnd   -> "End"
                          | k == Curses.keyHome  -> "Home"
                        _ -> show c

------------------------------------------------------------------------

-- | The time used and time left
instance Element PTimes where
    draw _ _ _ Nothing       = PTimes . Plain $ []
    draw (_,x) _ _ (Just fr) = PTimes $ Plain $! "  " ++elapsed++gap++remaining
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
    draw _ _ _ _ = PVersion $ P.pack versinfo

-- | Uptime
instance Element PTime where
    draw _ _ st _ = PTime . uptime $ st

-- | Play mode
instance Element PMode where
    draw _ _ st _ = PMode $! case status st of 
                        Stopped -> a
                        Paused  -> b
                        Playing -> c

        where a = P.packAddress "stop"#
              b = P.packAddress "pause"#
              c = P.packAddress "play"#

-- | Loop, normal or random
instance Element PMode2 where
    draw _ _ st _ = PMode2 $ case mode st of 
                        Random  -> a
                        Loop    -> b
                        Normal  -> c

        where a = P.packAddress "random"#
              b = P.packAddress "loop"#
              c = P.empty

------------------------------------------------------------------------

instance Element PlayModes where
    draw a b c d = PlayModes $  
        m `P.append` if m' == P.empty then P.empty else ' ' `P.cons` m'
        where
            (PMode  m ) = draw a b c d :: PMode
            (PMode2 m') = draw a b c d :: PMode2

instance Element PlayInfo where
    draw _ _ st _ = PlayInfo $ percent
        `P.append` P.packAddress " ("# 
        `P.append` P.pack (show . snd . bounds $ folders st)
        `P.append` P.packAddress " dir"# 
        `P.append` (if (snd . bounds $ folders st) == 1 then P.empty else P.packAddress "s"#) 
        `P.append` P.packAddress ", "# 
        `P.append` P.pack (show . size $ st)
        `P.append` P.packAddress " file"# 
        `P.append` (if size st == 1 then P.empty else P.packAddress "s"#) 
        `P.append` P.packAddress ")"#
      where
        curr   = cursor  st
        percent | percent' == 0  && curr == 0 = P.packAddress "top"#
                | percent' == 100             = P.packAddress "all"#
                | otherwise = if P.length s == 2 then ' ' `P.cons` s else s
            where 
                s = P.pack (show percent') `P.append` P.packAddress "%"#

        percent' :: Int = round $ 
                    ((fromIntegral curr) / 
                    ((fromIntegral . size $ st) - 1) * 100.0 :: Float)

instance Element PlayTitle where
    draw a@(_,x) b c d = PlayTitle $
        flip Fast hl $ space
             `P.append` inf
             `P.append` P.pack (replicate gapl ' ')
             `P.append` modes
             `P.append` P.pack (replicate gapr ' ')
             `P.append` time
             `P.append` space
             `P.append` ver
             `P.append` space
      where
        (PlayInfo inf)    = draw a b c d :: PlayInfo
        (PTime time)      = draw a b c d :: PTime
        (PlayModes modes) = draw a b c d :: PlayModes
        (PVersion ver)    = draw a b c d :: PVersion

        gap     = x - padding - P.length inf - modlen - P.length time - P.length ver
        gapl    = gap `div` 2
        gapr    = gap - gapl
        padding = 3
        modlen  = P.length modes
        space   = P.packAddress " "#
        hl     = highlight (style config)

-- | Playlist, TODO this should do threading-style rendering of filesystem trees
--
-- Rewrite the playlist code to draw trees.
--
instance Element PlayList where
    draw p@(y,x) q@(o,_) st z =
        PlayList $! title 
                 : list 
                 ++ (replicate (height - length list - 2) (Plain []))
                 ++ [minibuffer st]
        where
            (PlayTitle title)       = draw p q st z :: PlayTitle

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

            -- visible slice of the playlist
            visible = slice off (off + buflen) songs
                where off = screens * buflen

            -- todo: put dir on its own line
            visible' :: [(Maybe Int, P.FastString)]
            visible' = loop (-1) visible
                where  loop _ []     = []
                       loop n (v:vs) = 
                            let r = if fdir v > n then Just (fdir v) else Nothing
                            in (r,fbase v) : loop (fdir v) vs
                          
            list   = [ uncurry color n
                     | n <- zip (map drawIt visible') [0..] ]

            indent = (round $ (0.3 :: Float) * fromIntegral x) :: Int

            drawIt :: (Maybe Int, P.FastString) -> P.FastString
            drawIt (Nothing,v) = (P.pack $ replicate (1 + indent) ' ') `P.append` (mchop v)
            drawIt (Just i ,b) = d' `P.append` (mchop b)
                where
                    d = basenameP . dname $ folders st ! i
                    d' | P.length d > indent-1 = P.take (indent+1-4) d `P.append` (P.init ellipsis) `P.append` P.pack "/"
                       | otherwise             = d `P.append` ('/' `P.cons` spc)
                    spc = P.pack $ replicate (indent - P.length d) ' '
                
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

            mchop s | P.length s > (x-indent-4-1) = P.take (x-indent-4-1) s `P.append` ellipsis
                    | otherwise = s

--
-- | Decode the list of current tracks
--
printPlayList :: PlayList -> [StringA]
printPlayList (PlayList s) = s
                
------------------------------------------------------------------------

-- | Take two strings, and pad them in the middle
alignLR :: Int -> P.FastString -> P.FastString -> P.FastString
alignLR w l r | padding >= 0 = l `P.append` gap `P.append` r 
              | otherwise    = P.take (w - P.length r - 4) l `P.append` ellipsis `P.append` r
    where padding = w - P.length l - P.length r
          gap     = P.pack $ replicate padding ' '

ellipsis :: P.FastString
ellipsis = P.packAddress "... "#

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
redraw = 
   -- linux ncurses, in particular, seems to complain a lot. this is an easy solution
   Control.Exception.handle (\_ -> return ()) $ withState $ \s -> do

   sz@(h,w) <- screenSize
   f <- readClock id
   let x = {-# SCC "redraw.playscreen" #-} printPlayScreen (draw sz (0,0) s f :: PlayScreen)
       y = {-# SCC "redraw.playlist" #-} printPlayList   (draw sz (length x,0) s f :: PlayList)
       (PMode pm) = draw sz (0,0) s f :: PMode
       a = x ++ y

   -- set xterm title (should have an instance Element)
   when (xterm s) $ do
       setXtermTitle $ pm `P.append` 
            if status s == Playing
                then P.packAddress ": "# `P.append` 
                        case id3 s of
                            Nothing -> (fbase $ music s ! current s)
                            Just ti -> id3artist ti `P.append` 
                                       (P.packAddress ": "# `P.append` id3title ti)
                else P.empty
   
   gotoTop
   {-# SCC "redraw.draw" #-}mapM_ (\t -> do drawLine w t
                                            (y',x') <- Curses.getYX Curses.stdScr
                                            fillLine
                                            maybeLineDown t h y' x' )
         (take (h-1) (init a))

   when (helpVisible s) $ do
       let (HelpScreen help) = {-# SCC "redraw.help" #-} draw sz (0,0) s f :: HelpScreen
           (Fast fps _) = head help
           offset = (w - (P.length fps)) `div` 2
           height = (h - length help) `div` 2

       when (height > 0) $ do
            Curses.wMove Curses.stdScr ((h - length help) `div` 2) offset
            mapM_ (\t -> do drawLine w t
                            (y',_) <- Curses.getYX Curses.stdScr
                            Curses.wMove Curses.stdScr (y'+1) offset) help

   -- minibuffer

   Curses.wMove Curses.stdScr (h-1) 0
   fillLine 
   Curses.wMove Curses.stdScr (h-1) 0
   drawLine (w-1) (last a)
   when (miniFocused s) $ -- a fake cursor
        drawLine 1 (Fast (P.packAddress " "#) (helpscreen . style $  config))

------------------------------------------------------------------------
--
-- | Draw a coloured (or not) string to the screen
--
drawLine :: Int -> StringA -> IO ()

drawLine _ (Fancy s) =
    flip mapM_ s $ \fs -> case fs of
        C c     -> Curses.wAddChar Curses.stdScr c
        A c sty -> withStyle sty $ Curses.wAddChar Curses.stdScr c

drawLine _ (Plain s) = Curses.wAddStr Curses.stdScr s

drawLine _ (Fast ps sty) = withStyle sty $ P.unsafeUseAsCString ps $ \cstr -> 
    Curses.throwIfErr_ "drawLine"# $
        Curses.waddnstr Curses.stdScr cstr (fromIntegral . P.length $ ps)

------------------------------------------------------------------------

maybeLineDown :: StringA -> Int -> Int -> Int -> IO ()
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
fillLine = Control.Exception.catch (Curses.clrToEol) (\_ -> return ()) -- harmless?

--
-- | move cursor to origin of stdScr.
--
gotoTop :: IO ()
gotoTop = Curses.wMove Curses.stdScr 0 0


-- | Take a slice of an array efficiently
slice :: Int -> Int -> Array Int e -> [e]
slice i j arr = 
    let (a,b) = bounds arr
    in [unsafeAt arr n | n <- [max a i .. min b j] ]

------------------------------------------------------------------------

setXtermTitle :: P.FastString -> IO ()
setXtermTitle s = P.unsafeUseAsCString s $ c_setxterm

foreign import ccall safe "utils.h setxterm" c_setxterm :: CString -> IO ()
