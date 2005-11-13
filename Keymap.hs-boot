module Keymap where

import Data.FastPackedString

keymap :: [Char] -> [IO ()]

keyTable :: [(FastString, [Char], IO ())]
