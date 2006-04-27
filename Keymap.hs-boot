module Keymap where

import Data.ByteString

keymap :: [Char] -> [IO ()]

keyTable   :: [(ByteString, [Char], IO ())]
extraTable :: [(ByteString, [Char])]
