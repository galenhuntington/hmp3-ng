-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

-- For various reasons, ByteString is the lingua franca for this app.
-- This module provides basic text string functions.

module Text (
    u, matches,
    trim, guessEncoding, dropLastUTF8,
    readIntM,
    displayWidth, toMaxWidth, toWidth
) where

import Base

import Data.ByteString.Char8 qualified as P
import Data.ByteString.UTF8 qualified as UTF8
import Text.Regex.Posix (match, makeRegexOptsM, compIgnoreCase, compExtended)

import Foreign.C.Types (CWchar(..), CInt(..))

-- | Write u-strings like it's Python 2.
u :: String -> ByteString
u = UTF8.fromString

-- | Strip leading and trailing whitespace.
trim :: ByteString -> ByteString
trim = P.dropWhileEnd isSpace . P.dropSpace

-- | Swappable API for searching
-- TODO report invalid regex
matches :: ByteString -> ByteString -> Bool
matches s = maybe (const False) match $
    makeRegexOptsM (compIgnoreCase + compExtended) 0 s

readIntM :: ByteString -> Maybe Int
readIntM = fmap fst . P.readInt

-- | If seeming ISO-8859-1, convert to UTF-8.
guessEncoding :: ByteString -> ByteString
guessEncoding bs =
    if UTF8.replacement_char `elem` UTF8.toString bs
        then UTF8.fromString $ P.unpack bs
        else bs

-- | Drop last UTF-8 codepoint.
dropLastUTF8 :: ByteString -> ByteString
dropLastUTF8 = P.dropEnd 1 . P.dropWhileEnd isCB
    where isCB b = b >= '\128' && b < '\192'


-- Width-aware operations on UTF-8 'ByteString's, using libc 'wcwidth'.
-- A UTF-8 runtime locale is presumed; counts may differ otherwise.

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

