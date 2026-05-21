-- Copyright (c) 2004-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

-- | Width-aware operations on UTF-8 'ByteString's, using libc 'wcwidth' to
-- determine column widths.  Column counts therefore depend on the runtime
-- locale; expect deterministic results only under a UTF-8 locale.
module Width (
    displayWidth,
    ellipsize,
    forceWidth,
    charWidth,
  ) where

import Base

import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.UTF8 as UTF8

import Foreign.C.Types


-- | Sum of the column widths of every codepoint in a UTF-8 'ByteString'.
displayWidth :: ByteString -> Int
displayWidth = go 0 where
    go !acc bs = case UTF8.uncons bs of
        Nothing        -> acc
        Just (c, rest) -> go (acc + charWidth c) rest

-- | Truncate (with a trailing ellipsis) when the input does not fit in 'w'
-- columns; pass it through unchanged otherwise.
ellipsize :: Int -> ByteString -> ByteString
ellipsize = sizer False

-- | Like 'ellipsize', but pads short inputs with trailing spaces to 'w'
-- columns exactly.
forceWidth :: Int -> ByteString -> ByteString
forceWidth = sizer True

-- The two share an inner walker that finds the largest UTF-8 prefix whose
-- column count is < w, then fills the remaining columns with ellipses.
sizer :: Bool -> Int -> ByteString -> ByteString
sizer pad w bs
    | dw <= w   = if pad then bs <> P.replicate (w - dw) ' ' else bs
    | otherwise = walk 0 bs
  where
    dw = displayWidth bs

    walk !l rest = case UTF8.uncons rest of
        -- Unreachable: dw > w means we always hit the truncation branch
        -- before exhausting the input.  Fall back to the input as a safety
        -- net so a faulty width invariant never crashes the UI.
        Nothing -> bs
        Just (c, rest') ->
            let l' = l + charWidth c
            in if l' > w - 1
                then P.take (P.length bs - P.length rest) bs
                        <> mconcat (replicate (w - l) ellipsis)
                else walk l' rest'

ellipsis :: ByteString
ellipsis = UTF8.fromString "…"

charWidth :: Char -> Int
charWidth = fromIntegral . wcwidth . toEnum . fromEnum

foreign import ccall safe
    wcwidth :: CWchar -> CInt
