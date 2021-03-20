module Keymap where

import UI.HSCurses.Curses (Key)

keymap :: [Char] -> [IO ()]

keyTable   :: [(String, [Char], IO ())]
extraTable :: [(String, [Char])]
unkey      :: Key -> Char
charToKey  :: Char -> Key
