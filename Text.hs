-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

-- This module provides basic text string functions.

module Text (
    SText, matches,
    trim, spaces, guessEncoding, dropLastUTF8,
    readIntM, showInt, show2D,
    width, toMaxWidth, toWidth,
    fromBS, toBS, fromChar,
    notNull, encodeFS,isLineSafe,
) where

import Base

import Data.ByteString.Char8 qualified as P
import Data.ByteString.UTF8 qualified as UTF8
import Foreign.C.Types (CWchar(..), CInt(..))
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import Text.Regex.Posix (match, makeRegexOptsM, compIgnoreCase, compExtended, compNoSub)


-- SText type and functions.

-- | Screen/Sanitized/Safe text:
-- A string of valid UTF-8 with only printable characters.
data SText = SText
    { string :: !ByteString
    , width  :: !Int
    } deriving stock (Eq, Show)

instance Semigroup SText where
    s <> t = SText (s.string <> t.string) (s.width + t.width)
instance Monoid SText where
    mempty = SText "" 0
    mconcat l = SText (P.concat $ map (.string) l) (sum $ map (.width) l)
instance IsString SText where
    fromString s = let bs = UTF8.fromString $ toPrintable s in SText bs (stringWidth bs)

toBS :: SText -> ByteString
toBS = (.string)

width :: SText -> Int
width = (.width)

spaces :: Int -> SText
spaces n = SText (P.replicate n ' ') n

-- More convenient than null, I find.
notNull :: SText -> Bool
notNull = not . P.null . (.string)

-- | Swappable API for searching
matches :: SText -> Maybe (SText -> Bool)
matches (SText s _) =
    match' <$> makeRegexOptsM (compIgnoreCase + compExtended + compNoSub) 0 s
  where match' re (SText bs _) = match re bs

-- | Possible number.
readIntM :: SText -> Maybe Int
readIntM = fmap fst . P.readInt . toBS

showInt :: Int -> SText
showInt = fromAsciiBS . P.pack . show

-- | Show Int from 0 to 99 as two digits.
show2D :: Int -> SText
show2D n = SText (P.pack [dtc d1, dtc d0]) 2 where
    (d1, d0) = n `quotRem` 10
    dtc = toEnum . (48 +)

replacementChar :: Char
replacementChar =
    if charWidth UTF8.replacement_char == 1 then UTF8.replacement_char else '='

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
toPrintable = map \c -> if isPrintable c then c else replacementChar

-- | ByteString to displayable text.
-- Pre-checks for common case of already printable.
fromBS :: ByteString -> SText
fromBS bs = SText s (stringWidth s) where
    (_, bad) = UTF8.span (\c -> c /= UTF8.replacement_char && isPrintable c) bs
    s = if P.null bad then bs else UTF8.fromString $ toPrintable $ UTF8.toString bs

fromAsciiBS :: ByteString -> SText
fromAsciiBS s = SText s (P.length s)

fromChar :: Char -> SText
fromChar c = SText (UTF8.fromChar c') (charWidth c')
    where c' = if isPrintable c then c else replacementChar


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

-- | These functions truncate with ellipses if needed to get width ≤'w'.
-- 'toWidth' adds padding as needed so the width is exactly 'w'.
toMaxWidth, toWidth :: Int -> SText -> SText
toMaxWidth = sizer False
toWidth = sizer True

sizer :: Bool -> Int -> SText -> SText
sizer pad w s@(SText bs dw)
    | dw <= w = if pad then s <> spaces (w-dw) else s
    | True    = SText (walk 0 bs) w
  where
    walk !l rest
        | l' >= w = P.take (P.length bs - P.length rest) bs
                        <> mconcat (replicate (w-l) $ toBS "…")
        | True    = walk l' rest'
      where
        (c, rest') = fromJust $ UTF8.uncons rest -- can't be at end since dw>w
        l'         = l + charWidth c

stringWidth :: ByteString -> Int
stringWidth = UTF8.foldl (\acc c -> acc + charWidth c) 0

charWidth :: Char -> Int
charWidth = fromIntegral . wcwidth . toEnum . fromEnum

foreign import ccall unsafe
    wcwidth :: CWchar -> CInt

