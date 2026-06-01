module Keymap where

import UI.HSCurses.Curses (Key)

keyLoop :: IO ()

unkey      :: Key -> Char
charToKey  :: Char -> Key

