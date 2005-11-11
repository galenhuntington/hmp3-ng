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
-- | Colors and friends.
--

module Style where

import Data.Maybe
import Data.IORef
import System.IO.Unsafe
import qualified Curses

import Control.Exception        ( handle )

data UIStyle = UIStyle { window   :: Style
                       , highlight:: Style
                       , selected :: Style
                       , progress :: Style }

-- | Foreground and background color pairs
data Style = Style Color Color

-- | A List of characters with styles attached
data CharA = C {-# UNPACK #-} !Char
           | A {-# UNPACK #-} !Char !Style

-- | A list of such values
type StringA = [CharA]

--
-- our representation of colours
--
data Color
    = RGB (Int,Int,Int)
    | Default

--
-- Some basic colours (derivied from proxima/src/common/CommonTypes.hs)
--
black, grey, darkRed, red, darkGreen, green, brown, yellow          :: Color
darkBlue, blue, purple, magenta, darkCyan, cyan, white, brightWhite :: Color
black       = RGB (0,0,0)
grey        = RGB (128,128,128)
darkRed     = RGB (139,0,0)
red         = RGB (255,0,0)
darkGreen   = RGB (0,100,0)
green       = RGB (0,128,0)
brown       = RGB (165,42,42)
yellow      = RGB (255,255,0)
darkBlue    = RGB (0,0,139)
blue        = RGB (0,0,255)
purple      = RGB (128,0,128)
magenta     = RGB (255,0,255)
darkCyan    = RGB (0,139,139) 
cyan        = RGB (0,255,255)
white       = RGB (165,165,165)
brightWhite = RGB (255,255,255)

------------------------------------------------------------------------
--
-- | Set some colours, perform an action, and then reset the colours
--
withStyle :: Style -> (IO ()) -> IO ()
withStyle sty fn = uiAttr sty >>= setAttribute >> fn >> reset

--
-- | manipulate the current attributes of the standard screen
--
setAttribute :: (Curses.Attr, Curses.Pair) -> IO ()
setAttribute (a, p) = Curses.wAttrSet Curses.stdScr (a, p)

--
-- | Reset the screen to normal values
--
reset :: IO ()
reset = setAttribute (Curses.attr0, Curses.Pair 0)
    
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
initUiColors :: [Style] -> IO [((Curses.Color, Curses.Color), Curses.Pair)]
initUiColors stys = mapM (uncurry fn) (zip stys [1..])
    where
        fn :: Style -> Int -> IO ((Curses.Color, Curses.Color), Curses.Pair)
        fn sty p = do let (fg,bg) = style2curses sty
                          (_,fgc) = fg2attr fg
                          (_,bgc) = bg2attr bg
                      handle (\_ -> return ()) $ 
                           Curses.initPair (Curses.Pair p) fgc bgc
                      return ((fgc,bgc), (Curses.Pair p))

--
-- | Getting from nice abstract colours to ncurses-settable values
--
uiAttr :: Style -> IO (Curses.Attr, Curses.Pair)
uiAttr sty = do
    let (fg,bg)  = style2curses sty
        (a, fgc) = fg2attr fg
        (b, bgc) = bg2attr bg
    pair <- lookupPair (fgc, bgc)
    return (a `Curses.attrPlus` b, pair)

--
-- | retrieve a mapping from the pair map
--
lookupPair :: (Curses.Color, Curses.Color) -> IO (Curses.Pair)
lookupPair p = do pm <- readIORef pairMap
                  return $ case lookup p pm of
                        Nothing   -> Curses.Pair 0 -- default settings
                        Just pair -> pair

--
-- | map of Curses.Color pairs to ncurses terminal Pair settings
--
pairMap :: IORef [ ((Curses.Color, Curses.Color), Curses.Pair) ]
pairMap = unsafePerformIO $ newIORef []
{-# NOINLINE pairMap #-}

-- ---------------------------------------------------------------------
--
-- | mapping abstract colours to ncurses attributes and colours
--

bg2attr :: BackgroundColor -> (Curses.Attr, Curses.Color)
bg2attr c = case c of
    BlackB      -> (nullA, cblack)
    DarkRedB    -> (nullA, cred)
    DarkGreenB  -> (nullA, cgreen)
    BrownB      -> (nullA, cyellow)
    DarkBlueB   -> (nullA, cblue)
    PurpleB     -> (nullA, cmagenta)
    DarkCyanB   -> (nullA, ccyan)
    WhiteB      -> (nullA, cwhite)
    DefaultB    -> (nullA, defaultColor)

fg2attr :: ForegroundColor -> (Curses.Attr, Curses.Color)
fg2attr c = case c of
    BlackF       -> (nullA, cblack)
    GreyF        -> (boldA, cblack)
    DarkRedF     -> (nullA, cred)
    RedF         -> (boldA, cred)
    DarkGreenF   -> (nullA, cgreen)
    GreenF       -> (boldA, cgreen)
    BrownF       -> (nullA, cyellow)
    YellowF      -> (boldA, cyellow)
    DarkBlueF    -> (nullA, cblue)
    BlueF        -> (boldA, cblue)
    PurpleF      -> (nullA, cmagenta)
    MagentaF     -> (boldA, cmagenta)
    DarkCyanF    -> (nullA, ccyan)
    CyanF        -> (boldA, ccyan)
    WhiteF       -> (nullA, cwhite)
    BrightWhiteF -> (boldA, cwhite)
    DefaultF     -> (nullA, defaultColor)

------------------------------------------------------------------------
--
-- Basic (ncurses) colours.
--
defaultColor :: Curses.Color
defaultColor = fromJust $ Curses.color "default"

cblack, cred, cgreen, cyellow, cblue, cmagenta, ccyan, cwhite :: Curses.Color
cblack     = fromJust $ Curses.color "black"
cred       = fromJust $ Curses.color "red"
cgreen     = fromJust $ Curses.color "green"
cyellow    = fromJust $ Curses.color "yellow"
cblue      = fromJust $ Curses.color "blue"
cmagenta   = fromJust $ Curses.color "magenta"
ccyan      = fromJust $ Curses.color "cyan"
cwhite     = fromJust $ Curses.color "white"

--
-- Combine attribute with another attribute
--
setBoldA :: Curses.Attr -> Curses.Attr
setBoldA = flip Curses.setBold True

-- setUnderlineA, setDimA, setReverseA :: Curses.Attr -> Curses.Attr
-- setUnderlineA = flip Curses.setUnderline True
-- setDimA       = flip Curses.setDim       True
-- setReverseA   = flip Curses.setReverse   True

--
-- | Some attribute constants
--
boldA, nullA :: Curses.Attr
nullA       = Curses.attr0
boldA       = setBoldA      nullA

-- underlineA, dimA, reverseA :: Curses.Attr
-- underlineA  = setUnderlineA nullA
-- dimA        = setDimA       nullA
-- reverseA    = setReverseA   nullA

------------------------------------------------------------------------
--
-- Nicer, user-visible colour defs.
--
-- We separate colours into dark and bright colours, to prevent users
-- from erroneously constructing bright colours for dark backgrounds,
-- which doesn't work.

--
-- Foreground colours
--
data ForegroundColor
    = BlackF
    | GreyF
    | DarkRedF
    | RedF
    | DarkGreenF
    | GreenF
    | BrownF
    | YellowF
    | DarkBlueF
    | BlueF
    | PurpleF
    | MagentaF
    | DarkCyanF
    | CyanF
    | WhiteF
    | BrightWhiteF
    | DefaultF

--
-- Background colors can't be bright.
--
data BackgroundColor
    = BlackB
    | DarkRedB
    | DarkGreenB
    | BrownB
    | DarkBlueB
    | PurpleB
    | DarkCyanB
    | WhiteB
    | DefaultB

-- 
-- | Map Style rgb rgb colours to ncurses pairs
-- TODO a generic way to turn an rgb into the nearest curses color
--
style2curses :: Style -> (ForegroundColor, BackgroundColor)
style2curses (Style fg bg) = (fgCursCol fg, bgCursCol bg)
    where
        fgCursCol c = case c of
            RGB (0,0,0)         -> BlackF
            RGB (128,128,128)   -> GreyF
            RGB (139,0,0)       -> DarkRedF
            RGB (255,0,0)       -> RedF
            RGB (0,100,0)       -> DarkGreenF
            RGB (0,128,0)       -> GreenF
            RGB (165,42,42)     -> BrownF
            RGB (255,255,0)     -> YellowF
            RGB (0,0,139)       -> DarkBlueF
            RGB (0,0,255)       -> BlueF
            RGB (128,0,128)     -> PurpleF
            RGB (255,0,255)     -> MagentaF
            RGB (0,139,139)     -> DarkCyanF
            RGB (0,255,255)     -> CyanF
            RGB (165,165,165)   -> WhiteF
            RGB (255,255,255)   -> BrightWhiteF
            Default         -> DefaultF
            _               -> BlackF       -- NB

        bgCursCol c = case c of
            RGB (0,0,0)         -> BlackB
            RGB (128,128,128)   -> BlackB
            RGB (139,0,0)       -> DarkRedB
            RGB (255,0,0)       -> DarkRedB
            RGB (0,100,0)       -> DarkGreenB
            RGB (0,128,0)       -> DarkGreenB
            RGB (165,42,42)     -> BrownB
            RGB (255,255,0)     -> BrownB
            RGB (0,0,139)       -> DarkBlueB
            RGB (0,0,255)       -> DarkBlueB
            RGB (128,0,128)     -> PurpleB
            RGB (255,0,255)     -> PurpleB
            RGB (0,139,139)     -> DarkCyanB
            RGB (0,255,255)     -> DarkCyanB
            RGB (165,165,165)   -> WhiteB
            RGB (255,255,255)   -> WhiteB
            Default         -> DefaultB
            _               -> WhiteB

