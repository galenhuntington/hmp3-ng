module Keymap where

import Base
import UI.HSCurses.Curses (Key)

keymap :: [Char] -> [IO ()]

keyTable   :: [(ByteString, [Char], IO ())]
extraTable :: [(ByteString, [Char])]
unkey      :: Key -> Char
charToKey  :: Char -> Key
