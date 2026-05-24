{-# OPTIONS -Wno-orphans #-}

-- Copyright (c) 2004-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2008, 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

--
-- | Keymap manipulation.
--
-- Each "mode" of the keymap is a 'KeyMap': a closure that consumes one
-- keystroke and returns the 'KeyMap' to use for the next one.  Modal
-- transitions (entering search, popping up the song-history modal,
-- confirming a quit) are just "return a different 'KeyMap'."
--
module Keymap (keyLoop, keyTable, unkey, charToKey) where

import Base hiding ((!?))

import Core
import Config       (package)
import State        (getsHS, touchHS, modifyHS_, HState(histVisible, searchHist))
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
keyLoop = go mainMode where
    go (KeyMap f) = UI.getKey >>= \c -> clrmsg *> f c >>= go


------------------------------------------------------------------------
-- Top-level normal mode

mainMode :: KeyMap
mainMode = KeyMap dispatch where
    dispatch 'q'  = forcePause *> toggleExit *> touchHS $> confirmQuitMode
    dispatch c
        | c `elem` ['/', '?', '\\', '|']
                               = enterSearch c
        | c `elem` ['H', ';']  = showHist *> touchHS $> historyMode
        | c >= '1' && c <= '9' =
            jumpRel (0.1 * fromIntegral (fromEnum c - 48)) $> mainMode
        | True                 = sequence_ (M.lookup c keyMap) $> mainMode

    enterSearch stype = do
        toggleFocus
        hist <- getsHS searchHist
        searchMode stype $ Zipper "" hist []


------------------------------------------------------------------------
-- Search mode

-- | Zipper over the search-history list, with the currently edited
-- string in the focus.  'back' holds older entries we can step back
-- to (Up); 'front' holds entries we've stepped back from (Down).
data Zipper = Zipper { cur :: !String, _back :: ![String], _front :: ![String] }

searchMode :: Char -> Zipper -> IO KeyMap
searchMode stype = step where
    step z = renderSearch stype z $> KeyMap (`dispatch` z)

    dispatch c z
        | c == '\ESC'      = clrmsg *> touchHS *> leave
        | c `elem` enter'  = commit z
        | c `elem` delete' = step $ zipEdit dropLast z
        | k == KeyUp       = step $ zipUp z
        | k == KeyDown     = step $ zipDown z
        | k == KeyDC       = histDelete z
        | c > '\255'       = step z         -- ignore other special keys
        | otherwise        = step $ zipEdit (++ [c]) z
      where k = charToKey c

    commit (Zipper []  _ _) = clrmsg *> touchHS *> leave
    commit (Zipper pat _ _) = do
        let jumpy = if stype `elem` ['/', '?']
                    then jumpToMatchFile else jumpToMatchDir
        jumpy (Just pat) (stype `elem` ['/', '\\'])
        modifyHS_ \st -> st { searchHist = pat : filter (/= pat) (searchHist st) }
        leave

    histDelete z = do
        let z' = case z of
                Zipper _ b (pv:rest) -> Zipper pv b rest
                Zipper _ b _         -> Zipper "" b []
        modifyHS_ \st -> st { searchHist = filter (/= cur z) (searchHist st) }
        step z'

    leave = toggleFocus $> mainMode

renderSearch :: Char -> Zipper -> IO ()
renderSearch prefix z = do
    putmsg $ Fast (P.pack (prefix : cur z)) defaultSty
    touchHS

dropLast :: [a] -> [a]
dropLast [] = []
dropLast xs = init xs

zipEdit :: (String -> String) -> Zipper -> Zipper
zipEdit f z = z { cur = f (cur z) }

zipUp, zipDown :: Zipper -> Zipper
zipUp   (Zipper c (nx:rest) f)  = Zipper nx rest (c:f)
zipUp   z                       = z
zipDown (Zipper c b (pv:rest))  = Zipper pv (c:b) rest
zipDown z                       = z


------------------------------------------------------------------------
-- Song-history popup

historyMode :: KeyMap
historyMode = KeyMap \c -> do
    for_ (M.lookup c historyKeys) \k -> do
        phm <- getsHS histVisible
        for_ (phm >>= (!? k)) (jump . fst . snd)
    hideHist
    touchHS
    pure mainMode
  where
    historyKeys :: M.Map Char Int
    historyKeys = M.fromList $ zip (['0'..'9'] ++ ['a'..'z']) [0..]

    -- Compatibility: List.!? only added in GHC 9.8
    xs !? n = listToMaybe $ drop n xs


------------------------------------------------------------------------
-- Confirm-quit modal

confirmQuitMode :: KeyMap
confirmQuitMode = KeyMap \case
    'y' -> shutdown Nothing $> undefined -- shutdown never returns
    _   -> toggleExit *> touchHS $> mainMode


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

enter', delete' :: [Char]
enter'  = ['\n', '\r']
delete' = ['\BS', '\DEL', unkey KeyBackspace]


------------------------------------------------------------------------
-- The keymap with help descriptions and actions.

keyTable :: [(String, [Char], IO ())]
keyTable =
    [ ("Move up",                                 ['k',unkey KeyUp],    upOne)
    , ("Move down",                               ['j',unkey KeyDown],  downOne)
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

