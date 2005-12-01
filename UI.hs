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

        -- * Input
        getKey

  )   where

import Style
import FastIO                   (basenameP, replicatePS, printfPS)
import Tree                     (File(fdir, fbase), Dir(dname))
import State
import Syntax
import Config                   (config, versinfo, Config(style))
import qualified Curses

import {-# SOURCE #-} Keymap    (extraTable, keyTable)

import Data.IORef               (writeIORef)
import Data.List                (intersperse,isPrefixOf)
import Data.Array               ((!), bounds, Array)
import Data.Array.Base          (unsafeAt)
import System.IO                (stderr, hFlush)

import Control.Monad            (mapM_, when)
import qualified Control.Exception (catch, handle)

import System.Posix.Signals     (raiseSignal, sigTSTP)
import System.Posix.Env         (getEnv, putEnv)

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
            _ -> return ()

    Curses.initCurses resetui

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
end isXterm = do when isXterm $ setXtermTitle [P.pack "xterm"]
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
data PlayScreen = PlayScreen !PPlaying !ProgressBar !PTimes 

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
    draw w@(_,x') x st z = PPlaying . FancyS $ 
            [(spc2, defaultSty)
            ,(alignLR (x'-4) a b, defaultSty)]
        where
            (PId3 a)  = draw w x st z :: PId3 
            (PInfo b) = draw w x st z :: PInfo

-- | Id3 Info
instance Element PId3 where
    draw _ _ st _ = case id3 st of
        Just i  -> PId3 $ id3str i
        Nothing -> PId3 $ case size st of
                                0 -> emptyVal
                                _ -> fbase $ (music st) ! (current st)

-- | mp3 information
instance Element PInfo where
    draw _ _ st _ = PInfo $ case info st of
        Nothing  -> emptyVal
        Just i   -> userinfo i

emptyVal :: P.FastString
emptyVal = P.pack "(empty)"

spc2 :: P.FastString
spc2 = P.pack "  "

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
                    tot = round $! fromIntegral w *   (0.8::Float)
                    len = round $! fromIntegral tot * (0.2::Float)

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
    draw _ _ _ Nothing       = PTimes $ Fast (P.pack "     ") defaultSty
    draw (_,x) _ _ (Just fr) = PTimes $ FancyS $
                                [(spc2,     defaultSty)
                                ,(elapsed,  defaultSty)
                                ,(gap,      defaultSty)
                                ,(remaining,defaultSty)]
      where
        elapsed   = printfPS fmt1 lm lm'
        remaining = printfPS fmt2 rm rm'
        fmt1      = P.pack  "%01d:%02d" 
        fmt2      = P.pack "-%01d:%02d" 
        (lm,lm')  = quotRem (fst . currentTime $ fr) 60
        (rm,rm')  = quotRem (fst . timeLeft    $ fr) 60
        gap       = spaces distance
        distance  = x - 4 - P.length elapsed - P.length remaining

------------------------------------------------------------------------

-- | A progress bar
instance Element ProgressBar where
    draw (_,w) _ _ Nothing = ProgressBar . FancyS $
          [(spc2,defaultSty) ,(spaces (w-4), bgs)]
        where 
          (Style _ bg) = progress (style config)
          bgs          = Style bg bg

    draw (_,w) _ _ (Just fr) = ProgressBar . FancyS $
          [(spc2,defaultSty)
          ,((spaces distance),fgs)
          ,((spaces (width - distance)),bgs)]
        where 
          width    = w - 4
          total    = curr + left
          distance = round ((curr / total) * fromIntegral width)
          curr     = toFloat (currentTime fr)
          left     = toFloat (timeLeft fr)
          (Style fg bg) = progress (style config)
          bgs           = Style bg bg
          fgs           = Style fg fg

          toFloat (x,y) | x `seq` y `seq` False = undefined
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

        where a = P.pack "stop"
              b = P.pack "pause"
              c = P.pack "play"

-- | Loop, normal or random
instance Element PMode2 where
    draw _ _ st _ = PMode2 $ case mode st of 
                        Random  -> a
                        Loop    -> b
                        Normal  -> c

        where a = P.pack "random"
              b = P.pack "loop"
              c = P.empty

------------------------------------------------------------------------

instance Element PlayModes where
    draw a b c d = PlayModes $  
        m `P.append` if m' == P.empty then P.empty else ' ' `P.cons` m'
        where
            (PMode  m ) = draw a b c d :: PMode
            (PMode2 m') = draw a b c d :: PMode2

instance Element PlayInfo where     -- could FancyS it
    draw _ _ st _ = PlayInfo $ P.concat
         [percent
         ,P.pack " ("
         ,P.pack (show (1 + ( snd . bounds . folders $ st)))
         ,P.pack " dir"
         ,if (snd . bounds $ folders st) == 1 then P.empty else plural
         ,P.pack ", "
         ,P.pack (show . size $ st)
         ,P.pack " file"
         ,if size st == 1 then P.empty else plural
         ,P.pack ")"]
      where
        plural = P.pack "s"   -- expose to inlining
        pct    = P.pack "%"
        curr   = cursor  st
        percent | percent' == 0  && curr == 0 = P.pack "top"
                | percent' == 100             = P.pack "all"
                | otherwise = if P.length s == 2 then ' ' `P.cons` s else s
            where 
                s = P.pack (show percent') `P.append` pct

        percent' :: Int = round $ 
                    ((fromIntegral curr) / 
                    ((fromIntegral . size $ st) - 1) * 100.0 :: Float)

instance Element PlayTitle where
    draw a@(_,x) b c d =
        PlayTitle $ flip Fast hl $ P.concat 
              [space
              ,inf
              ,spaces gapl
              ,modes
              ,spaces gapr
              ,time
              ,space
              ,ver
              ,space]
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
        space   = P.pack " "
        hl     = highlight (style config)

-- | Playlist, TODO this should do threading-style rendering of filesystem trees
--
-- Rewrite the playlist code to draw trees.
--
instance Element PlayList where
    draw p@(y,x) q@(o,_) st z =
        PlayList $! title 
                 : list 
                 ++ (replicate (height - length list - 2) (Fast P.empty defaultSty))
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
                          
            -- problem: we color *after* merging with directories
         -- list   = [ uncurry color n
         --          | n <- zip (map drawIt visible') [0..] ]

            list   = [ drawIt . color . mchop $ n | n <- zip visible' [0..] ]

            indent = (round $ (0.3 :: Float) * fromIntegral x) :: Int
                
            color :: ((Maybe Int,P.FastString),Int) -> (Maybe Int, StringA)
            color ((m,s),i) 
                | i == select && i == playing = f sty3
                | i == select                 = f sty2
                | i == playing                = f sty1
                | otherwise                   = (m,Fast s defaultSty)
                where
                    f sty = (m, Fast (s `P.append` 
                                        (spaces (x-indent-1-P.length s)))
                                sty)
            
            sty1 = selected . style $ config
            sty2 = cursors  . style  $ config
            sty3 = combined . style $ config

            -- must mchop before drawing.
            drawIt :: (Maybe Int, StringA) -> StringA
            drawIt (Nothing,Fast v sty) = 
                Fast ((spaces (1 + indent)) `P.append` v) sty

            drawIt (Just i,Fast b sty) = FancyS [pref, post]
              where
                pref = (d', if sty == sty2 || sty == sty3 then sty2 else defaultSty)
                post = (b,sty)

                d   = basenameP $ case size st of
                                    0 -> P.pack "(empty)"
                                    _ -> dname $ folders st ! i

                spc = spaces (indent - P.length d)

                d' | P.length d > indent-1 
                   = P.concat [ P.take (indent+1-4) d 
                              , (P.init ellipsis) 
                              , fwd ]

                   | otherwise = P.concat [ d, fwd, spc ]

            drawIt _ = error "UI.drawIt: color gaves us a non-Fast StringA!"

            fwd      = P.pack "/"

            mchop :: ((Maybe Int,P.FastString),Int) -> ((Maybe Int,P.FastString),Int) 
            mchop a@((i,s),j)
                | P.length s > (x-indent-4-1) 
                = ((i, P.take (x-indent-4-1) s `P.append` ellipsis),j)
                | otherwise = a

--
-- | Decode the list of current tracks
--
printPlayList :: PlayList -> [StringA]
printPlayList (PlayList s) = s
{-# INLINE printPlayList #-}
                
------------------------------------------------------------------------

-- | Take two strings, and pad them in the middle
alignLR :: Int -> P.FastString -> P.FastString -> P.FastString
alignLR w l r | padding >= 0 = P.concat [l, gap, r]
              | otherwise    = P.take (w - P.length r - 4) $ P.concat [l, ellipsis, r]
    where padding = w - P.length l - P.length r
          gap     = spaces padding

spaces :: Int -> P.FastString
spaces n = replicatePS n ' '
{-# INLINE spaces #-}

ellipsis :: P.FastString
ellipsis = P.pack "... "
{-# INLINE ellipsis #-}

------------------------------------------------------------------------
--
-- | Now write out just the clock line
-- Speed things up a bit, just use read State.
--
redrawJustClock :: IO ()
redrawJustClock = do 
   Control.Exception.handle (\_ -> return ()) $ do

   st      <- readState id
   fr      <- readClock id
   s@(_,w) <- screenSize
   let (ProgressBar bar) = draw s undefined st fr :: ProgressBar
       (PTimes times)    = {-# SCC "redrawJustClock.times" #-} draw s undefined st fr :: PTimes
   Curses.wMove Curses.stdScr 1 0   -- hardcoded!
   drawLine w bar
   Curses.wMove Curses.stdScr 2 0   -- hardcoded!
   drawLine w times
   drawHelp st fr s

------------------------------------------------------------------------
--
-- work for drawing help. draw the help screen if it is up
--
drawHelp :: State -> Maybe Frame -> (Int,Int) -> IO ()
drawHelp st fr s@(h,w) =
   when (helpVisible st) $ do
       let (HelpScreen help) = draw s (0,0) st fr :: HelpScreen
           (Fast fps _)      = head help
           offset            = (w - (P.length fps)) `div` 2
           height            = (h - length help) `div` 2
       when (height > 0) $ do
            Curses.wMove Curses.stdScr ((h - length help) `div` 2) offset
            mapM_ (\t -> do drawLine w t
                            (y',_) <- Curses.getYX Curses.stdScr
                            Curses.wMove Curses.stdScr (y'+1) offset) help

------------------------------------------------------------------------
--
-- | Draw the screen
--
redraw :: IO ()
redraw = 
   -- linux ncurses, in particular, seems to complain a lot. this is an easy solution
   Control.Exception.handle (\_ -> return ()) $ do

   s <- readState id    -- another refresh could be triggered?
   f <- readClock id
   sz@(h,w) <- screenSize

   let x = printPlayScreen (draw sz (0,0) s f :: PlayScreen)
       y = printPlayList   (draw sz (length x,0) s f :: PlayList)
       a = x ++ y

   -- set xterm title (should have an instance Element)
   when (xterm s) $ do
       setXtermTitle $ 
            if status s == Playing
                then case id3 s of
                        Nothing -> case size s of
                                        0 -> [P.pack "hmp3"]
                                        _ -> [(fbase $ music s ! current s)]
                        Just ti -> id3artist ti :
                                   if P.null (id3title ti) 
                                        then [] 
                                        else [P.pack ": ", id3title ti]

                else let (PMode pm) = draw sz (0,0) s f :: PMode in [pm]
   
   gotoTop
   mapM_ (\t -> do drawLine w t
                   (y',x') <- Curses.getYX Curses.stdScr
                   fillLine
                   maybeLineDown t h y' x' )
         (take (h-1) (init a))
   drawHelp s f sz

   -- minibuffer

   Curses.wMove Curses.stdScr (h-1) 0
   fillLine 
   Curses.wMove Curses.stdScr (h-1) 0
   drawLine (w-1) (last a)
   when (miniFocused s) $ -- a fake cursor
        drawLine 1 (Fast (P.pack " ") (helpscreen . style $  config))

------------------------------------------------------------------------
--
-- | Draw a coloured (or not) string to the screen
--
drawLine :: Int -> StringA -> IO ()
drawLine _ (Fast ps sty) = drawPackedString ps sty
drawLine _ (FancyS []) = return ()
drawLine _ (FancyS ls) = loop ls
    where loop []             = return ()
          loop ((l,sty):xs)   = drawPackedString l sty >> loop xs

-- worker
drawPackedString :: P.FastString -> Style -> IO ()
drawPackedString ps sty = 
    withStyle sty $ P.unsafeUseAsCString ps $ \cstr -> 
        Curses.throwIfErr_ msg $
            Curses.waddnstr Curses.stdScr cstr (fromIntegral . P.length $ ps)
    where
        msg = P.packAddress "drawPackedString"#


------------------------------------------------------------------------

maybeLineDown :: StringA -> Int -> Int -> Int -> IO ()
maybeLineDown (Fast s _) h y _ | s == P.empty = lineDown h y
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
{-# INLINE gotoTop #-}

-- | Take a slice of an array efficiently
slice :: Int -> Int -> Array Int e -> [e]
slice i j arr = 
    let (a,b) = bounds arr
    in [unsafeAt arr n | n <- [max a i .. min b j] ]
{-# INLINE slice #-}

------------------------------------------------------------------------

--
-- | magics for setting xterm titles using ansi escape sequences
--
setXtermTitle :: [P.FastString] -> IO ()
setXtermTitle strs = do
    mapM_ (P.hPut stderr) (before : strs ++ [after])
    hFlush stderr 
  where
    before = P.pack "\ESC]0;"
    after  = P.pack "\007"
