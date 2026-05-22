-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

-- | Width-aware operations on UTF-8 'ByteString's, using libc 'wcwidth'.
-- A UTF-8 runtime locale is presumed; counts may differ otherwise.
module Width (displayWidth, toMaxWidth, toWidth) where

import Base

import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.UTF8 as UTF8

import Foreign.C.Types


-- | Sum of the column widths of every codepoint in a UTF-8 'ByteString'.
displayWidth :: ByteString -> Int
displayWidth = UTF8.foldl (\acc c -> acc + charWidth c) 0

-- | These functions truncate with ellipses if needed to get width ≤'w'.
-- 'toWidth' adds padding as needed so the width is exactly 'w'.
toMaxWidth, toWidth :: Int -> ByteString -> ByteString
toMaxWidth = sizer False
toWidth = sizer True

sizer :: Bool -> Int -> ByteString -> ByteString
sizer pad w bs
    | dw <= w = if pad then bs <> P.replicate (w-dw) ' ' else bs
    | True    = walk 0 bs
  where
    dw = displayWidth bs
    walk !l rest
        | l' >= w = P.take (P.length bs - P.length rest) bs
                        <> mconcat (replicate (w-l) $ UTF8.fromString "…")
        | True    = walk l' rest'
      where
        (c, rest') = fromJust $ UTF8.uncons rest -- can't be at end since dw>w
        l'         = l + charWidth c

charWidth :: Char -> Int
charWidth = fromIntegral . wcwidth . toEnum . fromEnum

foreign import ccall safe
    wcwidth :: CWchar -> CInt

