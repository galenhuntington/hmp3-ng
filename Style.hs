-- Copyright (c) 2004-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2022, 2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

-- | Color manipulation

module Style where

import Base
import UI.HSCurses.Curses qualified as Curses
import Data.Map qualified as M

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

-- | A terminal color: the terminal default, reverse-video, or one of the
-- eight ANSI hues at normal or bright intensity.  (Bright is rendered with
-- the bold attribute, which is how 8-color terminals expose it.)
data Color = Default | Reverse | Color !Intensity !Hue
    deriving stock (Eq, Ord, Show)

data Intensity = Normal | Bright
    deriving stock (Eq, Ord, Show)

data Hue = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    deriving stock (Eq, Ord, Show)

-- | Foreground and background color pairs
data Style = Style !Color !Color
    deriving stock (Eq,Ord)

-- | A styled UTF-8 ByteString segment.
data Segment = Seg !Style {-# UNPACK #-} !ByteString

-- | A line of segments.
type Line = [Segment]

------------------------------------------------------------------------
-- | Named colors for the config file and the built-in styles.  The
-- \"dark\" name of each pair is the normal-intensity hue; the plain name
-- is its bright variant (so @red@ is bright, @darkred@ is normal).
stringToColor :: String -> Maybe Color
stringToColor s = case map toLower s of
    "black"         -> Just $ Color Normal Black
    "grey"          -> Just $ Color Bright Black
    "darkred"       -> Just $ Color Normal Red
    "red"           -> Just $ Color Bright Red
    "darkgreen"     -> Just $ Color Normal Green
    "green"         -> Just $ Color Bright Green
    "brown"         -> Just $ Color Normal Yellow
    "yellow"        -> Just $ Color Bright Yellow
    "darkblue"      -> Just $ Color Normal Blue
    "blue"          -> Just $ Color Bright Blue
    "purple"        -> Just $ Color Normal Magenta
    "magenta"       -> Just $ Color Bright Magenta
    "darkcyan"      -> Just $ Color Normal Cyan
    "cyan"          -> Just $ Color Bright Cyan
    "white"         -> Just $ Color Normal White
    "brightwhite"   -> Just $ Color Bright White
    "default"       -> Just Default
    "reverse"       -> Just Reverse
    _               -> Nothing

------------------------------------------------------------------------
-- | Set some colours, perform an action, and then reset the colours
withStyle :: Style -> IO () -> IO ()
withStyle sty fn = uiAttr sty >>= setAttribute >> fn >> reset
{-# INLINE withStyle #-}

-- | manipulate the current attributes of the standard screen
-- Only set attr if it's different to the current one?
setAttribute :: (Curses.Attr, Curses.Pair) -> IO ()
setAttribute = uncurry Curses.attrSet

-- | Reset the screen to normal values
reset :: IO ()
reset = setAttribute (Curses.attr0, Curses.Pair 0)

-- | And turn on the colours
initcolours :: UIStyle -> IO ()
initcolours sty = do
    let ls  = [sty.modals, sty.warnings, sty.window,
               sty.selected, sty.titlebar, sty.progress,
               sty.blockcursor, sty.cursors, sty.combined ]
        Style fg bg = sty.progress    -- bonus style
    pairs <- initUiColors (ls ++ [Style bg bg, Style fg fg])
    writeIORef pairMap pairs
    -- set the background
    uiAttr sty.window >>= \(_,p) -> Curses.bkgrndSet nullA p

------------------------------------------------------------------------
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
-- | Getting from nice abstract colours to ncurses-settable values

-- 20% of allocss occur here! But there's only 3 or 4 colours :/
-- Every call to uiAttr
uiAttr :: Style -> IO (Curses.Attr, Curses.Pair)
uiAttr sty = do
    m <- readIORef pairMap
    pure $ lookupPair m sty

-- | Given a curses color pair, find the Curses.Pair (i.e. the pair
-- curses thinks these colors map to) from the state
lookupPair :: PairMap -> Style -> (Curses.Attr, Curses.Pair)
lookupPair m s =
    fromMaybe (Curses.attr0, Curses.Pair 0) (M.lookup s m)

-- | Keep a map of nice style defs to underlying curses pairs, created at init time
type PairMap = M.Map Style (Curses.Attr, Curses.Pair)

-- | map of Curses.Color pairs to ncurses terminal Pair settings
pairMap :: IORef PairMap
pairMap = unsafePerformIO $ newIORef M.empty
{-# NOINLINE pairMap #-}

------------------------------------------------------------------------
-- Basic (ncurses) colours.

defaultColor :: Curses.Color
defaultColor = fromJust $ Curses.color "default"

-- Combine attribute with another attribute
setBoldA, setReverseA ::  Curses.Attr -> Curses.Attr
setBoldA     = flip Curses.setBold    True
setReverseA  = flip Curses.setReverse True

-- | Some attribute constants
boldA, nullA, reverseA :: Curses.Attr
nullA       = Curses.attr0
boldA       = setBoldA      nullA
reverseA    = setReverseA   nullA

------------------------------------------------------------------------

newtype CColor = CColor (Curses.Attr, Curses.Color)

-- | Map an abstract 'Style' to its ncurses foreground/background pair.
style2curses :: Style -> (CColor, CColor)
style2curses (Style fg bg) = (fgCursCol fg, bgCursCol bg)

-- | The ncurses color for each ANSI hue.
hueColor :: Hue -> Curses.Color
hueColor = fromJust . Curses.color . map toLower . show

-- | Foreground: bright hues take the bold attribute.
fgCursCol :: Color -> CColor
fgCursCol = \case
    Default        -> CColor (nullA, defaultColor)
    Reverse        -> CColor (reverseA, defaultColor)
    Color Bright h -> CColor (boldA, hueColor h)
    Color Normal h -> CColor (nullA, hueColor h)

-- | Background: a terminal can't embolden a background, so intensity is
-- dropped here.
bgCursCol :: Color -> CColor
bgCursCol = \case
    Default   -> CColor (nullA, defaultColor)
    Reverse   -> CColor (reverseA, defaultColor)
    Color _ h -> CColor (nullA, hueColor h)

defaultSty :: Style
defaultSty = Style Default Default

style :: String -> String -> Style
style a b = let f = fromJust . stringToColor in Style (f a) (f b)

plainSeg :: ByteString -> Segment
plainSeg = Seg defaultSty

------------------------------------------------------------------------
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

-- | Read style.conf, and construct a UIStyle from it, to insert into
buildStyle :: Config -> UIStyle
buildStyle bs = UIStyle {
         window      = f bs.hmp3_window
       , modals      = f bs.hmp3_modals
       , titlebar    = f bs.hmp3_titlebar
       , selected    = f bs.hmp3_selected
       , cursors     = f bs.hmp3_cursors
       , combined    = f bs.hmp3_combined
       , warnings    = f bs.hmp3_warnings
       , blockcursor = f bs.hmp3_blockcursor
       , progress    = f bs.hmp3_progress
    }
    where 
        f (x,y) = Style (g x) (g y)
        g x     = fromMaybe Default $ stringToColor x

-- Built-in styles

defaultStyle :: UIStyle
defaultStyle  = UIStyle
    { window     = style "default"      "default"
    , titlebar   = style "brightwhite"  "green"
    , selected   = style "blue"         "default"
    , cursors    = style "black"        "cyan"
    , combined   = style "brightwhite"  "cyan"
    , warnings   = style "red"          "default"
    , modals     = style "black"        "white"
    , blockcursor= style "black"        "red"
    , progress   = style "cyan"         "white"
    }

monoStyle :: UIStyle
monoStyle = UIStyle
    { window      = style "default"     "default"
    , titlebar    = style "reverse"     "reverse"
    , selected    = style "brightwhite" "default"
    , cursors     = style "reverse"     "reverse"
    , combined    = style "reverse"     "reverse"
    , warnings    = style "reverse"     "reverse"
    , modals      = style "reverse"     "reverse"
    , blockcursor = style "reverse"     "reverse"
    , progress    = style "reverse"     "reverse"
    }

