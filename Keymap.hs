--
-- Copyright (c) 2004-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2008, 2019-2026 Galen Huntington
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
-- | Keymap manipulation.
--
-- Each "mode" of the keymap is a 'KeyMap': a closure that consumes one
-- keystroke and returns the 'KeyMap' to use for the next one.  Modal
-- transitions (entering search, popping up the song-history modal,
-- confirming a quit) are just "return a different 'KeyMap'."
--
module Keymap (keyLoop, keyTable, unkey, charToKey) where

import Base

import Core
import Config       (package)
import State        (getsST, touchST, HState(histVisible))
import Style        (defaultSty, StringA(Fast))
import qualified UI (getKey, resetui)

import UI.HSCurses.Curses (Key(..), decodeKey)

import qualified Data.ByteString.Char8 as P
import qualified Data.Map.Strict as M


------------------------------------------------------------------------
-- The keymap driver

-- | A 'KeyMap' handles the next keystroke and produces the 'KeyMap' to
-- use thereafter.
newtype KeyMap = KeyMap (Char -> IO KeyMap)

-- | Read keys forever and dispatch.  Each round clears the minibuffer
-- between the keystroke and the action so messages from the previous
-- action remain visible until the user reacts.
keyLoop :: IO ()
keyLoop = go (mainMode []) where
    go (KeyMap f) = UI.getKey >>= \c -> clrmsg *> f c >>= go


------------------------------------------------------------------------
-- Top-level normal mode

-- | Normal-mode dispatch.  The argument is the cross-search persistent
-- search history, threaded through every mode.
mainMode :: [String] -> KeyMap
mainMode hist = KeyMap dispatch where
    dispatch '/'  = enterSearch SearchFiles Forwards  '/'
    dispatch '?'  = enterSearch SearchFiles Backwards '?'
    dispatch '\\' = enterSearch SearchDirs  Forwards  '\\'
    dispatch '|'  = enterSearch SearchDirs  Backwards '|'
    dispatch 'q'  = enterConfirmQuit
    dispatch c
        | c `elem` ['H', ';']  = enterHistory
        | c >= '1' && c <= '9' = stay $ jumpRel (0.1 * fromIntegral (fromEnum c - 48))
        | True                 = stay $ sequence_ (M.lookup c keyMap)

    stay action = action $> mainMode hist

    enterSearch kind dir prefix = do
        toggleFocus
        putmsg $ Fast (P.singleton prefix) defaultSty
        touchST
        pure $ searchMode hist kind dir prefix (Zipper "" hist [])

    enterHistory = do
        showHist
        touchST
        pure $ historyMode hist

    enterConfirmQuit = do
        forcePause
        toggleExit
        touchST
        pure $ confirmQuitMode hist


------------------------------------------------------------------------
-- Search mode

data Direction  = Forwards | Backwards deriving stock Eq
data SearchKind = SearchFiles | SearchDirs

-- | Zipper over the search-history list, with the currently edited
-- string in the focus.  'back' holds older entries we can step back
-- to (Up); 'front' holds entries we've stepped back from (Down).
data Zipper = Zipper { cur :: !String, _back :: ![String], _front :: ![String] }

searchMode :: [String] -> SearchKind -> Direction -> Char -> Zipper -> KeyMap
searchMode hist kind dir prefix = step where
    step z = KeyMap (`dispatch` z)

    dispatch c z
        | c == '\ESC'      = endSearch (clrmsg *> touchST) hist
        | c `elem` enter'  = commit z
        | c `elem` delete' = repaint (zipEdit dropLast z)
        | c == upChar      = repaint (zipUp z)
        | c == downChar    = repaint (zipDown z)
        | c == dcChar      = histDelete z
        | c > '\255'       = pure (step z)         -- ignore other special keys
        | otherwise        = repaint (zipEdit (++ [c]) z)

    repaint z' = renderSearch prefix z' $> step z'

    commit (Zipper []  _ _) = endSearch (clrmsg *> touchST) hist
    commit (Zipper pat _ _) = do
        let jumpy = case kind of
                SearchFiles -> jumpToMatchFile
                SearchDirs  -> jumpToMatch
        jumpy (Just pat) (dir == Forwards)
        endSearch (pure ()) (pat : filter (/= pat) hist)

    histDelete z = do
        let newhist = filter (/= cur z) hist
            z' = case z of
                Zipper _ b (pv:rest) -> Zipper pv b rest
                Zipper _ b _         -> Zipper "" b []
        renderSearch prefix z'
        pure $ searchMode newhist kind dir prefix z'

    endSearch action hist' = do
        action
        toggleFocus
        pure $ mainMode hist'

renderSearch :: Char -> Zipper -> IO ()
renderSearch prefix z = do
    putmsg $ Fast (P.pack (prefix : cur z)) defaultSty
    touchST

zipEdit :: (String -> String) -> Zipper -> Zipper
zipEdit f z = z { cur = f (cur z) }

dropLast :: [a] -> [a]
dropLast [] = []
dropLast xs = init xs

zipUp, zipDown :: Zipper -> Zipper
zipUp   (Zipper c (nx:rest) f)  = Zipper nx rest (c:f)
zipUp   z                       = z
zipDown (Zipper c b (pv:rest))  = Zipper pv (c:b) rest
zipDown z                       = z


------------------------------------------------------------------------
-- Song-history popup

historyMode :: [String] -> KeyMap
historyMode hist = KeyMap \c -> do
    for_ (M.lookup c historyKeys) \k -> do
        phm <- getsST histVisible
        for_ (phm >>= safeIndex k) (jump . fst . snd)
    hideHist
    touchST
    pure $ mainMode hist
  where
    historyKeys :: M.Map Char Int
    historyKeys = M.fromList $ zip (['0'..'9'] ++ ['a'..'z']) [0..]

    -- listToMaybe . drop, compatible with GHCs before List.!? (9.8).
    safeIndex n xs = listToMaybe (drop n xs)


------------------------------------------------------------------------
-- Confirm-quit modal

confirmQuitMode :: [String] -> KeyMap
confirmQuitMode hist = KeyMap \case
    'y' -> quit Nothing $> mainMode hist           -- quit never returns
    _   -> toggleExit *> touchST $> mainMode hist


------------------------------------------------------------------------
-- Char ↔ Key translation
--
-- ncurses delivers special keys as integer codes ≥ 256; for everything
-- in 0..255 'decodeKey' returns 'KeyChar (chr n)'.  We keep working in
-- 'Char' (UI.getKey's type), so we extend the range up to '\500' to
-- cover the named keys we actually use (KEY_RESIZE is around 410).

deriving stock instance Ord Key

charToKey :: Char -> Key
charToKey = decodeKey . toEnum . fromEnum

keyCharMap :: M.Map Key Char
keyCharMap = M.fromList [(charToKey c, c) | c <- ['\0' .. '\500']]

unkey :: Key -> Char
unkey k = fromMaybe '\0' $ M.lookup k keyCharMap

upChar, downChar, dcChar :: Char
upChar   = unkey KeyUp
downChar = unkey KeyDown
dcChar   = unkey KeyDC

enter', delete' :: [Char]
enter'  = ['\n', '\r']
delete' = ['\BS', '\DEL', unkey KeyBackspace]


------------------------------------------------------------------------
-- The keymap with help descriptions and actions.

keyTable :: [(String, [Char], IO ())]
keyTable =
    [ ("Move up",                                 ['k',unkey KeyUp],    up)
    , ("Move down",                               ['j',unkey KeyDown],  down)
    , ("Page down",                               [unkey KeyNPage],     downPage)
    , ("Page up",                                 [unkey KeyPPage],     upPage)
    , ("Jump to start of list",                   [unkey KeyHome,'0'],  jump 0)
    , ("Jump to end of list",                     [unkey KeyEnd,'G'],   jump maxBound)
    , ("Jump to 10%, 20%, 30%, etc., point",      ['1','2','3'],        placeholder)
    , ("Seek left within song",                   [unkey KeyLeft],      seekLeft)
    , ("Seek right within song",                  [unkey KeyRight],     seekRight)
    , ("Toggle pause",                            [' '],                pause)
    , ("Play song under cursor",                  ['\n'],               play)
    , ("Play previous track",                     ['K'],                playPrev)
    , ("Play next track",                         ['J'],                playNext)
    , ("Toggle the help screen",                  ['h'],                toggleHelp)
    , ("Jump to currently playing song",          ['t'],                jumpToPlaying)
    , ("Select and play next track",              ['d'],                playNext *> jumpToPlaying)
    , ("Cycle through normal, random, loop, and single modes",
                                                  ['m'],                nextMode)
    , ("Refresh the display",                     ['\^L'],              UI.resetui)
    , ("Repeat last regex search",                ['n'],                jumpToMatchFile Nothing True)
    , ("Repeat last regex search backwards",      ['N'],                jumpToMatchFile Nothing False)
    , ("Play",                                    ['p'],                playCur)
    , ("Mark for deletion in .hmp3-delete",       ['D'],                blacklist)
    , ("Load config file",                        ['l'],                loadConfig)
    , ("Restart song",                            [unkey KeyBackspace], seekStart)
    , ("Toggle the song history",                 ['H', ';'],           placeholder)
    , ("Search for file matching regex",          ['/'],                placeholder)
    , ("Search backwards for file",               ['?'],                placeholder)
    , ("Search for directory matching regex",     ['\\'],               placeholder)
    , ("Search backwards for directory",          ['|'],                placeholder)
    , ("Quit " ++ package,                        ['q'],                placeholder)
    ]
  where placeholder = pure () -- handled separately

-- Compiled dispatch table for normal-mode single-key commands.
keyMap :: M.Map Char (IO ())
keyMap = M.fromList [ (c, a) | (_, cs, a) <- keyTable, c <- cs ]

