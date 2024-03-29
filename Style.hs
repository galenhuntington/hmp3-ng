-- 
-- Copyright (c) 2004-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2021 Galen Huntington
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

import Base
import qualified UI.HSCurses.Curses as Curses
import qualified Data.Map as M  (fromList, empty, lookup, Map)

------------------------------------------------------------------------

-- | User-configurable colours
-- Each component of this structure corresponds to a fg\/bg colour pair
-- for an item in the ui
data UIStyle = UIStyle {
     window      :: !Style  -- default window colour
   , modals      :: !Style  -- help screen
   , titlebar    :: !Style  -- titlebar of window
   , selected    :: !Style  -- currently playing track
   , cursors     :: !Style  -- the scrolling cursor line
   , combined    :: !Style  -- the style to use when the cursor is on the current track
   , warnings    :: !Style  -- style for warnings
   , blockcursor :: !Style  -- style for the block cursor when typing text
   , progress    :: !Style  -- style for the progress bar
   }

------------------------------------------------------------------------

-- | Colors 
data Color
    = RGB {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    | Default
    | Reverse
    deriving stock (Eq,Ord)

-- | Foreground and background color pairs
data Style = Style !Color !Color 
    deriving stock (Eq,Ord)

-- | Can hold an optimized ByteString or a Unicode String.
data AmbiString = B !ByteString | U !String

-- | A list of such values (the representation is optimised)
data StringA 
    = Fast   {-# UNPACK #-} !ByteString {-# UNPACK #-} !Style
    | FancyS ![(AmbiString,Style)]  -- one line made up of segments

------------------------------------------------------------------------
--
-- | Some simple colours (derivied from proxima\/src\/common\/CommonTypes.hs)
--
-- But we don't have a light blue?
--
black, grey, darkred, red, darkgreen, green, brown, yellow          :: Color
darkblue, blue, purple, magenta, darkcyan, cyan, white, brightwhite :: Color
black       = RGB 0 0 0
grey        = RGB 128 128 128
darkred     = RGB 139 0 0
red         = RGB 255 0 0
darkgreen   = RGB 0 100 0
green       = RGB 0 128 0
brown       = RGB 165 42 42
yellow      = RGB 255 255 0
darkblue    = RGB 0 0 139
blue        = RGB 0 0 255
purple      = RGB 128 0 128
magenta     = RGB 255 0 255
darkcyan    = RGB 0 139 139 
cyan        = RGB 0 255 255
white       = RGB 165 165 165
brightwhite = RGB 255 255 255

defaultfg, defaultbg, reversefg, reversebg :: Color
defaultfg   = Default
defaultbg   = Default
reversefg   = Reverse
reversebg   = Reverse

--
-- | map strings to colors
--
stringToColor :: String -> Maybe Color
stringToColor s = case map toLower s of
    "black"         -> Just black
    "grey"          -> Just grey
    "darkred"       -> Just darkred
    "red"           -> Just red
    "darkgreen"     -> Just darkgreen
    "green"         -> Just green
    "brown"         -> Just brown
    "yellow"        -> Just yellow
    "darkblue"      -> Just darkblue
    "blue"          -> Just blue
    "purple"        -> Just purple
    "magenta"       -> Just magenta
    "darkcyan"      -> Just darkcyan
    "cyan"          -> Just cyan
    "white"         -> Just white
    "brightwhite"   -> Just brightwhite
    "default"       -> Just Default
    "reverse"       -> Just Reverse
    _               -> Nothing

------------------------------------------------------------------------
--
-- | Set some colours, perform an action, and then reset the colours
--
withStyle :: Style -> IO () -> IO ()
withStyle sty fn = uiAttr sty >>= setAttribute >> fn >> reset
{-# INLINE withStyle #-}

--
-- | manipulate the current attributes of the standard screen
-- Only set attr if it's different to the current one?
--
setAttribute :: (Curses.Attr, Curses.Pair) -> IO ()
setAttribute = uncurry Curses.attrSet
{-# INLINE setAttribute #-}

--
-- | Reset the screen to normal values
--
reset :: IO ()
reset = setAttribute (Curses.attr0, Curses.Pair 0)
{-# INLINE reset #-}

--
-- | And turn on the colours
--
initcolours :: UIStyle -> IO ()
initcolours sty = do
    let ls  = [modals sty, warnings sty, window sty,
               selected sty, titlebar sty, progress sty,
               blockcursor sty, cursors sty, combined sty ]
        (Style fg bg) = progress sty    -- bonus style

    pairs <- initUiColors (ls ++ [Style bg bg, Style fg fg])
    writeIORef pairMap pairs
    -- set the background
    uiAttr (window sty) >>= \(_,p) -> Curses.bkgrndSet nullA p

------------------------------------------------------------------------
--
-- | Set up the ui attributes, given a ui style record
--
-- Returns an association list of pairs for foreground and bg colors,
-- associated with the terminal color pair that has been defined for
-- those colors.
--
initUiColors :: [Style] -> IO PairMap
initUiColors stys = do 
    ls <- sequence [ uncurry fn m | m <- zip stys [1..] ]
    pure (M.fromList ls)
  where
    fn :: Style -> Int -> IO (Style, (Curses.Attr,Curses.Pair))
    fn sty p = do
        let (CColor (a,fgc),CColor (b,bgc)) = style2curses sty
        discardErrors $ Curses.initPair (Curses.Pair p) fgc bgc
        pure (sty, (a `Curses.attrPlus` b, Curses.Pair p))

------------------------------------------------------------------------
--
-- | Getting from nice abstract colours to ncurses-settable values

-- 20% of allocss occur here! But there's only 3 or 4 colours :/
-- Every call to uiAttr
--
uiAttr :: Style -> IO (Curses.Attr, Curses.Pair)
uiAttr sty = do
    m <- readIORef pairMap
    pure $ lookupPair m sty
{-# INLINE uiAttr #-}

-- | Given a curses color pair, find the Curses.Pair (i.e. the pair
-- curses thinks these colors map to) from the state
lookupPair :: PairMap -> Style -> (Curses.Attr, Curses.Pair)
lookupPair m s =
    fromMaybe (Curses.attr0, Curses.Pair 0) (M.lookup s m)
{-# INLINE lookupPair #-}

-- | Keep a map of nice style defs to underlying curses pairs, created at init time
type PairMap = M.Map Style (Curses.Attr, Curses.Pair)

-- | map of Curses.Color pairs to ncurses terminal Pair settings
pairMap :: IORef PairMap
pairMap = unsafePerformIO $ newIORef M.empty
{-# NOINLINE pairMap #-}

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
setBoldA, setReverseA ::  Curses.Attr -> Curses.Attr
setBoldA     = flip Curses.setBold    True
setReverseA  = flip Curses.setReverse True

--
-- | Some attribute constants
--
boldA, nullA, reverseA :: Curses.Attr
nullA       = Curses.attr0
boldA       = setBoldA      nullA
reverseA    = setReverseA   nullA

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
    Reverse           -> CColor (reverseA, defaultColor)
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
    Reverse           -> CColor (reverseA, defaultColor)
    _                 -> CColor (nullA, cwhite)    -- NB

defaultSty :: Style
defaultSty = Style Default Default

------------------------------------------------------------------------
--
-- Support for runtime configuration
-- We choose a simple strategy, read/showable record types, with strings
-- to represent colors
--
-- The fields must map to UIStyle
--
-- It is this data type that is stored in 'show' format in style.conf
--
data Config = Config {
         hmp3_window      :: (String,String)
       , hmp3_modals      :: (String,String)
       , hmp3_titlebar    :: (String,String)
       , hmp3_selected    :: (String,String)
       , hmp3_cursors     :: (String,String)
       , hmp3_combined    :: (String,String)
       , hmp3_warnings    :: (String,String)
       , hmp3_blockcursor :: (String,String)
       , hmp3_progress    :: (String,String)
     } deriving stock (Show,Read)

--
-- | Read style.conf, and construct a UIStyle from it, to insert into
--
buildStyle :: Config -> UIStyle
buildStyle bs = UIStyle {
         window      = f $ hmp3_window      bs
       , modals      = f $ hmp3_modals      bs
       , titlebar    = f $ hmp3_titlebar    bs
       , selected    = f $ hmp3_selected    bs
       , cursors     = f $ hmp3_cursors     bs
       , combined    = f $ hmp3_combined    bs
       , warnings    = f $ hmp3_warnings    bs
       , blockcursor = f $ hmp3_blockcursor bs
       , progress    = f $ hmp3_progress    bs
    }

    where 
        f (x,y) = Style (g x) (g y)
        g x     = fromMaybe Default $ stringToColor x
