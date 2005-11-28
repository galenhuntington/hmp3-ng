-- 
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

--
-- | Color manipulation
--

module Style where

import qualified Curses
import qualified Data.FastPackedString as P (FastString)

import Data.Word                (Word8)
import Data.Maybe               (fromJust)
import Data.IORef               (readIORef, newIORef, IORef)
import qualified Data.Map as M  (fromList, empty, lookup, Map)

import System.IO.Unsafe         (unsafePerformIO)
import Control.Exception        (handle)

------------------------------------------------------------------------

-- | User-configurable colours
data UIStyle = UIStyle { window     :: !Style
                       , highlight  :: !Style
                       , selected   :: !Style
                       , cursors    :: !Style
                       , combined   :: !Style
                       , warnings   :: !Style
                       , helpscreen :: !Style
                       , progress   :: !Style }

-- | Foreground and background color pairs
data Style = Style {-# UNPACK #-} !Color !Color
    deriving Eq

-- | A List of characters with styles attached
data CharA = C {-# UNPACK #-} !Char
           | A {-# UNPACK #-} !Char !Style

-- | A list of such values (the representation is optimised)
data StringA = Fancy {-# UNPACK #-} ![CharA]   -- lines with colours in them
             | Plain {-# UNPACK #-} !String    -- plain text, no attributes set
             | Fast  {-# UNPACK #-} !P.FastString !Style

instance Show StringA where
  show (Fancy cs)   = show $ map (\c -> case c of C d -> d ; A d _ -> d) cs
  show (Plain cs)   = show cs
  show (Fast  cs _) = show cs

data Color
    = RGB {-# UNPACK #-} !Word8 !Word8 !Word8
    | Default
    deriving Eq

------------------------------------------------------------------------
--
-- | Some simple colours (derivied from proxima/src/common/CommonTypes.hs)
--
black, grey, darkRed, red, darkGreen, green, brown, yellow          :: Color
darkBlue, blue, purple, magenta, darkCyan, cyan, white, brightWhite :: Color
black       = RGB 0 0 0
grey        = RGB 128 128 128
darkRed     = RGB 139 0 0
red         = RGB 255 0 0
darkGreen   = RGB 0 100 0
green       = RGB 0 128 0
brown       = RGB 165 42 42
yellow      = RGB 255 255 0
darkBlue    = RGB 0 0 139
blue        = RGB 0 0 255
purple      = RGB 128 0 128
magenta     = RGB 255 0 255
darkCyan    = RGB 0 139 139 
cyan        = RGB 0 255 255
white       = RGB 165 165 165
brightWhite = RGB 255 255 255

------------------------------------------------------------------------
--
-- | Set some colours, perform an action, and then reset the colours
--
withStyle :: Style -> (IO ()) -> IO ()
withStyle sty fn = uiAttr sty >>= setAttribute >> fn >> reset
{-# INLINE withStyle #-}

--
-- | manipulate the current attributes of the standard screen
--
setAttribute :: (Curses.Attr, Curses.Pair) -> IO ()
setAttribute = Curses.wAttrSet Curses.stdScr
{-# INLINE setAttribute #-}

--
-- | Reset the screen to normal values
--
reset :: IO ()
reset = setAttribute (Curses.attr0, Curses.Pair 0)
{-# INLINE reset #-}
    
------------------------------------------------------------------------
--
-- | Set up the ui attributes, given a ui style record
--
-- Returns an association list of pairs for foreground and bg colors,
-- associated with the terminal color pair that has been defined for
-- those colors.
--
-- TODO remember to update this if new fields are added to the ui
--
initUiColors :: [Style] -> IO (M.Map (Curses.Color, Curses.Color) Curses.Pair)
initUiColors stys = do 
    ls <- sequence [ uncurry fn m | m <- zip stys [1..] ]
    return (M.fromList ls)
    where
        fn :: Style -> Int -> IO ((Curses.Color, Curses.Color), Curses.Pair)
        fn sty p = case style2curses sty of
            (CColor (_,fgc), CColor (_,bgc)) -> do
                  handle (\_ -> return ()) $    -- ignore any problems
                       Curses.initPair (Curses.Pair p) fgc bgc
                  return ((fgc,bgc), (Curses.Pair p))

------------------------------------------------------------------------
--
-- | Getting from nice abstract colours to ncurses-settable values
--
uiAttr :: Style -> IO (Curses.Attr, Curses.Pair)
uiAttr sty = case style2curses sty of 
        (CColor (a,fgc), CColor (b,bgc)) -> do 
            m <- readIORef pairMap
            return $! (a `Curses.attrPlus` b, lookupPair m (fgc,bgc))
{-# INLINE uiAttr #-}

-- | Given a curses color pair, find the Curses.Pair (i.e. the pair
-- curses thinks these colors map to) from the state
lookupPair :: PairMap -> (Curses.Color, Curses.Color) -> Curses.Pair
lookupPair m p = case M.lookup p m of
                    Nothing   -> Curses.Pair 0 -- default settings
                    Just pair -> pair
{-# INLINE lookupPair #-}

type PairMap = M.Map (Curses.Color, Curses.Color) Curses.Pair

-- | map of Curses.Color pairs to ncurses terminal Pair settings
pairMap :: IORef PairMap
pairMap = unsafePerformIO $ newIORef (M.empty)
{-# NOINLINE pairMap #-}

------------------------------------------------------------------------
--
-- Basic (ncurses) colours.
--
defaultColor :: Curses.Color
defaultColor = fromJust $! Curses.color "default"

cblack, cred, cgreen, cyellow, cblue, cmagenta, ccyan, cwhite :: Curses.Color
cblack     = fromJust $! Curses.color "black"
cred       = fromJust $! Curses.color "red"
cgreen     = fromJust $! Curses.color "green"
cyellow    = fromJust $! Curses.color "yellow"
cblue      = fromJust $! Curses.color "blue"
cmagenta   = fromJust $! Curses.color "magenta"
ccyan      = fromJust $! Curses.color "cyan"
cwhite     = fromJust $! Curses.color "white"

--
-- Combine attribute with another attribute
--
setBoldA, setReverseA :: Curses.Attr -> Curses.Attr
setBoldA      = flip Curses.setBold    True
setReverseA   = flip Curses.setReverse True

--
-- | Some attribute constants
--
boldA, nullA :: Curses.Attr
nullA       = Curses.attr0
boldA       = setBoldA      nullA

------------------------------------------------------------------------

newtype CColor = CColor (Curses.Attr, Curses.Color)
-- 
-- | Map Style rgb rgb colours to ncurses pairs
-- TODO a generic way to turn an rgb into the nearest curses color
--
style2curses :: Style -> (CColor, CColor)
style2curses (Style fg bg) = (fgCursCol fg, bgCursCol bg)
{-# INLINE style2curses #-}

fgCursCol :: Color -> CColor
fgCursCol c = case c of
    RGB 0 0 0         -> CColor (nullA, cblack)
    RGB 128 128 128   -> CColor (boldA, cblack)
    RGB 139 0 0       -> CColor (nullA, cred)
    RGB 255 0 0       -> CColor (boldA, cred)
    RGB 0 100 0       -> CColor (nullA, cgreen)
    RGB 0 128 0       -> CColor (boldA, cgreen)
    RGB 165 42 42     -> CColor (nullA, cyellow)
    RGB 255 255 0     -> CColor (boldA, cyellow)
    RGB 0 0 139       -> CColor (nullA, cblue)
    RGB 0 0 255       -> CColor (boldA, cblue)
    RGB 128 0 128     -> CColor (nullA, cmagenta)
    RGB 255 0 255     -> CColor (boldA, cmagenta)
    RGB 0 139 139     -> CColor (nullA, ccyan)
    RGB 0 255 255     -> CColor (boldA, ccyan)
    RGB 165 165 165   -> CColor (nullA, cwhite)
    RGB 255 255 255   -> CColor (boldA, cwhite)
    Default           -> CColor (nullA, defaultColor)
    _                 -> CColor (nullA, cblack) -- NB

bgCursCol :: Color -> CColor
bgCursCol c = case c of
    RGB 0 0 0         -> CColor (nullA, cblack)
    RGB 128 128 128   -> CColor (nullA, cblack)
    RGB 139 0 0       -> CColor (nullA, cred)
    RGB 255 0 0       -> CColor (nullA, cred)
    RGB 0 100 0       -> CColor (nullA, cgreen)
    RGB 0 128 0       -> CColor (nullA, cgreen)
    RGB 165 42 42     -> CColor (nullA, cyellow)
    RGB 255 255 0     -> CColor (nullA, cyellow)
    RGB 0 0 139       -> CColor (nullA, cblue)
    RGB 0 0 255       -> CColor (nullA, cblue)
    RGB 128 0 128     -> CColor (nullA, cmagenta)
    RGB 255 0 255     -> CColor (nullA, cmagenta)
    RGB 0 139 139     -> CColor (nullA, ccyan)
    RGB 0 255 255     -> CColor (nullA, ccyan)
    RGB 165 165 165   -> CColor (nullA, cwhite)
    RGB 255 255 255   -> CColor (nullA, cwhite)
    Default           -> CColor (nullA, defaultColor)
    _                 -> CColor (nullA, cwhite)    -- NB

