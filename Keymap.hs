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
module Keymap (keyLoop, keyTable, unkey, charToKey, dropLastUTF8) where

import Base

import Core
import Config (package)
import Keyboard (unkey, charToKey, Key(..), historyKeys)
import State (getsHS, modifyHS_, KeysHelp, Modal(..), HState(..))
import Style (defaultSty, StringA(Fast))
import UI qualified (getKey, resetui)

import Data.ByteString.Char8 qualified as P
import Data.ByteString.UTF8 qualified as UTF8
import Data.Map.Strict qualified as M


------------------------------------------------------------------------
-- The keymap driver

-- | A 'KeyMap' handles the next keystroke and produces the 'KeyMap' to
-- use thereafter.
newtype KeyMap = KeyMap (Char -> IO KeyMap)

-- | Read keys forever and dispatch.  Each round clears the minibuffer
-- between the keystroke and the action so messages from the previous
-- action remain visible until the user reacts.
keyLoop :: IO Void
keyLoop = go mainMode where
    go (KeyMap f) = UI.getKey >>= \c -> clearMessage *> f c >>= go


------------------------------------------------------------------------
-- Top-level normal mode

mainMode :: KeyMap
mainMode = KeyMap \c -> getsHS modal >>= \case

    Just ExitModal
        | c `elem` ['y', 'Y', '\^C'] -> shutdown Nothing $> undefined
        | True                       -> closeModal $> mainMode

    Just (HistModal hist) -> do
        for_ (M.lookup c historyKeyMap >>= (hist !?)) (jump . fst . snd)
        closeModal $> mainMode

    _ -> if
        | c `elem` ['/', '?', '\\', '|'] -> do
            toggleFocus
            hist <- getsHS searchHist
            searchMode c $ Zipper "" hist []
        | c `elem` ['q', '\^C'] ->
            forcePause *> setsModal (const $ Just ExitModal) $> mainMode
        | c `elem` ['H', ';'] ->
            showHist $> mainMode
        | c >= '1' && c <= '9' ->
            jumpRel (fromIntegral (fromEnum c - 48) / 10) $> mainMode
        | True -> sequence_ (M.lookup c keyMap) $> mainMode


historyKeyMap :: M.Map Char Int
historyKeyMap = M.fromList $ zip (toList historyKeys) [0..]


------------------------------------------------------------------------
-- Search mode

searchMode :: Char -> Zipper ByteString -> IO KeyMap
searchMode stype = step where
    step z = renderSearch stype z $> KeyMap (`dispatch` z)

    dispatch c z
        | c `elem` ['\ESC', '\^C']
                           = clearMessage *> leave
        | c `elem` enter'  = commit z
        | c `elem` delete' = step $ zipEdit dropLastUTF8 z
        | k == KeyUp       = step $ zipUp z
        | k == KeyDown     = step $ zipDown z
        | k == KeyDC       = histDelete z
        | c < ' ' || c > '\255'
                           = step z   -- ignore other special keys
        | otherwise        = step $ zipEdit (`P.snoc` c) z
      where k = charToKey c

    commit (Zipper ""  _ _) = clearMessage *> leave
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
        modifyHS_ \st -> st { searchHist = filter (/= zipperCur z) (searchHist st) }
        step z'

    leave = toggleFocus $> mainMode

renderSearch :: Char -> Zipper ByteString -> IO ()
renderSearch prefix z = putMessage $ Fast (prefix `P.cons` zipperCur z) defaultSty

dropLastUTF8 :: ByteString -> ByteString
dropLastUTF8 = P.dropEnd 1 . P.dropWhileEnd isCB
    where isCB b = b >= '\128' && b < '\192'

enter', delete' :: [Char]
enter'  = ['\n', '\r']
delete' = ['\BS', '\DEL', unkey KeyBackspace]


------------------------------------------------------------------------
-- The keymap with help descriptions and actions.

keyTable :: [(ByteString, [Char], IO ())]
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
    , ("Play from cursor",                        ['\n'],               playCursor)
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
    , ("Quit " <> UTF8.fromString package,        ['q'],                placeholder)
    ]
  where placeholder = pure () -- handled separately

-- Compiled dispatch table for normal-mode single-key commands.
keyMap :: M.Map Char (IO ())
keyMap = M.fromList [ (c, a) | (_, cs, a) <- keyTable, c <- cs ]

keysHelp :: [KeysHelp]
keysHelp = [ (keys, desc) | (desc, keys, _) <- keyTable ]

toggleHelp :: IO ()
toggleHelp = setsModal \st ->
    if isNothing $ modal st then Just $ HelpModal keysHelp else Nothing

