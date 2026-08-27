-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

-- For various reasons, ByteString is the lingua franca for this app.
-- This module provides basic text string functions.

module Text (
    u, matches,
    trim, spaces, guessEncoding, dropLastUTF8,
    readIntM, showInt,
    displayWidth, toMaxWidth, toWidth,
    toText, isLineSafe,
    encodeFS,
) where

import Base

import Data.ByteString.Char8 qualified as P
import Data.ByteString.UTF8 qualified as UTF8
import Foreign.C.Types (CWchar(..), CInt(..))
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import Text.Regex.Posix (match, makeRegexOptsM, compIgnoreCase, compExtended)


-- | Write u-strings like it's Python 2.
u :: String -> ByteString
u = UTF8.fromString

-- | Strip leading and trailing whitespace.
trim :: ByteString -> ByteString
trim = P.dropWhileEnd isSpace . P.dropSpace

spaces :: Int -> ByteString
spaces = flip P.replicate ' '

-- | Swappable API for searching
matches :: ByteString -> Maybe (ByteString -> Bool)
matches s = match <$> makeRegexOptsM (compIgnoreCase + compExtended) 0 s

readIntM :: ByteString -> Maybe Int
readIntM = fmap fst . P.readInt

showInt :: Int -> ByteString
showInt = P.pack . show

-- This might save memory in the most common case.
dedup :: (ByteString -> ByteString) -> ByteString -> ByteString
dedup f a = let b = f a in if a == b then a else b

-- | If seeming ISO-8859-1, convert to UTF-8.
guessEncoding :: ByteString -> ByteString
guessEncoding = dedup \bs -> UTF8.fromString $ map toPrintable $
    let s = UTF8.toString bs
    in if UTF8.replacement_char `elem` s then P.unpack bs else s

-- | Drop last UTF-8 codepoint.
dropLastUTF8 :: ByteString -> ByteString
dropLastUTF8 = P.dropEnd 1 . P.dropWhileEnd isCB
    where isCB b = b >= '\128' && b < '\192'

-- XXX when we drop GHC 9.4 we can use its filepath's function
-- | Filesystem encoding for CLI (PEP 383).
encodeFS :: String -> IO ByteString
encodeFS str = do
    enc <- getFileSystemEncoding
    GHC.withCStringLen enc str P.packCStringLen

-- | Can file be sent to decoder?
isLineSafe :: ByteString -> Bool
isLineSafe = P.all (`notElem` ['\0', '\r', '\n'])

-- | Blot out control and other unprintable characters.
toPrintable :: Char -> Char
toPrintable c
    | c == '\0' || charWidth c < 0 = UTF8.replacement_char
    | True                         = c

-- | ByteString to displayable text.
toText :: ByteString -> ByteString
toText = dedup $ UTF8.fromString . map toPrintable . UTF8.toString

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

foreign import ccall unsafe
    wcwidth :: CWchar -> CInt

