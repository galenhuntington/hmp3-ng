module Keymap where

import UI.HSCurses.Curses (Key)

keyLoop :: IO ()

keyTable   :: [(String, [Char], IO ())]
unkey      :: Key -> Char
charToKey  :: Char -> Key

