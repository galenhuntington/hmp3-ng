-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

-- This module provides basic text string functions.

module Text (
    SText, matches,
    trim, spaces, guessEncoding, dropLastUTF8,
    readIntM, showInt,
    displayWidth, toMaxWidth, toWidth, byteLength,
    fromBS, isLineSafe,
    encodeFS,
    drawText, setXtermTitle,
    toBS, -- only used in test suite
) where

import Base

import Data.ByteString.Char8 qualified as P
import Data.ByteString.Unsafe qualified as P
import Data.ByteString.UTF8 qualified as UTF8
import Foreign.C.Types (CWchar(..), CInt(..))
import Foreign.C.String
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import System.IO (stderr, hFlush)
import Text.Regex.Posix (match, makeRegexOptsM, compIgnoreCase, compExtended, compNoSub)
import UI.HSCurses.Curses qualified as Curses

-- SText type and functions.

-- | Screen/Sanitized/Safe text:
-- A string of valid UTF-8 with only printable characters.
newtype SText = SText ByteString
    deriving stock (Eq, Ord, Show)
    deriving newtype (Semigroup, Monoid)

-- | Convenient internal combinator.
toBS :: SText -> ByteString
toBS (SText bs) = bs

-- | Can be used in lieu of 'displayWidth' for known 1-width-character text.
byteLength :: SText -> Int
byteLength = P.length . toBS

instance IsString SText where
    fromString = SText . UTF8.fromString . toPrintable

spaces :: Int -> SText
spaces = SText . flip P.replicate ' '

-- | Swappable API for searching
matches :: ByteString -> Maybe (SText -> Bool)
matches s = match' <$> makeRegexOptsM (compIgnoreCase + compExtended + compNoSub) 0 s
    where match' re (SText bs) = match re bs  -- TODO a combinator for this?

-- | Possible number.
readIntM :: SText -> Maybe Int
readIntM = fmap fst . P.readInt . toBS

showInt :: Int -> SText
showInt = SText . P.pack . show

-- | If seeming ISO-8859-1, convert to UTF-8.
guessEncoding :: ByteString -> SText
guessEncoding bs =
    if UTF8.replacement_char `elem` UTF8.toString bs
        then fromString $ P.unpack bs else fromBS bs

-- | Test if printable according to wcwidth.
isPrintable :: Char -> Bool
isPrintable c = c /= '\0' && charWidth c >= 0

-- | Blot out control and other unprintable characters.
toPrintable :: String -> String
toPrintable = map \c -> if isPrintable c then c else UTF8.replacement_char

-- | ByteString to displayable text.
-- Pre-checks for common case of already printable.
fromBS :: ByteString -> SText
fromBS bs = SText $
    if P.null bad then bs else UTF8.fromString $ toPrintable $ UTF8.toString bs
  where
    (_, bad) = UTF8.span (\c -> c /= UTF8.replacement_char && isPrintable c) bs


-- ByteString utilities.

-- | Strip leading and trailing whitespace.
trim :: ByteString -> ByteString
trim = P.dropWhileEnd isSpace . P.dropSpace

-- | Drop last UTF-8 codepoint.
dropLastUTF8 :: ByteString -> ByteString
dropLastUTF8 = P.dropEnd 1 . P.dropWhileEnd isCB
    where isCB b = b >= '\128' && b < '\192'

-- | Can file be sent to decoder?
isLineSafe :: ByteString -> Bool
isLineSafe = P.all (`notElem` ['\0', '\r', '\n'])

-- XXX when we drop GHC 9.4 we can use its filepath's function
-- | Filesystem encoding for CLI (PEP 383).
encodeFS :: String -> IO ByteString
encodeFS str = do
    enc <- getFileSystemEncoding
    GHC.withCStringLen enc str P.packCStringLen


-- Width operations on 'SText', using libc 'wcwidth'.
-- A UTF-8 runtime locale is presumed; counts may differ otherwise.

-- | Sum of the column widths of every codepoint.
displayWidth :: SText -> Int
displayWidth = UTF8.foldl (\acc c -> acc + charWidth c) 0 . toBS

-- | These functions truncate with ellipses if needed to get width ≤'w'.
-- 'toWidth' adds padding as needed so the width is exactly 'w'.
toMaxWidth, toWidth :: Int -> SText -> SText
toMaxWidth = sizer False
toWidth = sizer True

sizer :: Bool -> Int -> SText -> SText
sizer pad w s@(SText bs)
    | dw <= w = if pad then s <> spaces (w-dw) else s
    | True    = walk 0 bs
  where
    dw = displayWidth s
    byteTake i = SText . P.take i . toBS
    walk !l rest
        | l' >= w = byteTake (byteLength s - P.length rest) s
                        <> mconcat (replicate (w-l) "…")
        | True    = walk l' rest'
      where
        (c, rest') = fromJust $ UTF8.uncons rest -- can't be at end since dw>w
        l'         = l + charWidth c

charWidth :: Char -> Int
charWidth = fromIntegral . wcwidth . toEnum . fromEnum

foreign import ccall unsafe
    wcwidth :: CWchar -> CInt


-- Curses output

-- | Set xterm title with ANSI escape sequence.
setXtermTitle :: [SText] -> IO ()
setXtermTitle strs = do
    traverse_ (P.hPut stderr) (before : map toBS strs ++ [after])
    hFlush stderr
  where
    before = "\ESC]0;"
    after  = "\007"

-- | Draw text to Curses.  Safe because C only reads the bytes.
drawText :: SText -> IO ()
drawText (SText bs) = void $
    P.unsafeUseAsCStringLen bs \(cstr, len) ->
        waddnstr Curses.stdScr cstr (fromIntegral len)

foreign import ccall safe
    waddnstr :: Curses.Window -> CString -> CInt -> IO CInt

