-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

module Elements where

import Base
import Decoder (Frame(..))
import Keyboard (charToKey, historyKeys)
import State
import Text
import Paths_hmp3_ng (version)

import Data.ByteString.Char8 qualified as P
import System.Clock
import UI.HSCurses.Curses qualified as Curses


package :: String
package = "hmp3-ng"

fullVersion :: String
fullVersion  = package ++ " v" ++ showVersion version

-- | Version info
pVersion :: ByteString
pVersion = P.pack fullVersion

commonModalWidth :: Int -> Int
commonModalWidth w = max (min w 3) $ round $ fromIntegral w * (0.8::Float)

showClock :: Fixed E2 -> ByteString
showClock t =
    let m, si, sd :: Int
        (m, s) = t `divMod'` 60
        si     = floor s
        sd     = floor (s*10) `mod` 10
    in P.pack $ printf "%d:%02d.%d" m si sd

-- | Human-friendly duration, with a flag to include seconds.
showDuration :: Bool -> TimeSpec -> ByteString
showDuration showSecs tm
    | ms == 0 && showSecs
              = go ""
    | hs == 0 = go $ printf "%dm" m
    | d == 0  = go $ printf "%dh%02dm" h m
    | True    = go $ printf "%dd%02dh%02dm" d h m
  where
    go      = P.pack . ss
    (ms, s) = sec tm `quotRem` 60
    (hs, m) = ms `quotRem` 60
    (d, h)  = hs `quotRem` 24
    ss      =
        if showSecs then (<> printf (if ms > 0 then "%02ds" else "%ds") s) else id

-- | The time used and time left
pTimes :: Int -> Maybe Frame -> ByteString
pTimes w clock
    | w - 4 < P.length elapsed = ""
    | True                     =
        mconcat $ ["  ", elapsed] ++ [gap <> "-" <> left | distance > 0]
  where
    elapsed  = showClock (maybe 0 (.currentTime) clock)
    left     = maybe "?:??.?" (showClock . (.timeLeft)) clock
    gap      = spaces distance
    distance = w - 5 - P.length elapsed - P.length left

-- | Progress out of total
progress :: Int -> Maybe Frame -> Int
progress width = maybe 0 \Frame {..} ->
    let total    = curr + toRational timeLeft - ε
        curr     = toRational currentTime
        ε        = toRational (toEnum 1 `asTypeOf` currentTime) / 2
    in ceiling (curr * fromIntegral (width - 1) / total)

-- | Given a width and size of left, center, and right elements, determine
-- whether left and right can fit, padding between, and amount of center to show
fitLCR :: Int -> (Int, Int, Int) -> (Bool, Int, Int, Int)
fitLCR w (lsz, csz, rsz) =
    let sides = w - csz
        side = sides `div` 2
        gap  = sides - lsz - rsz
    in if gap >= 2
        then let gapl = 1 `max` ((side - lsz) `min` (gap - 1))
             in (True, gapl, gap - gapl, csz)
        else (False, side `max` 1, (sides - side) `max` ((w-1) `min` 1),
                0 `max` (csz `min` (w - 2)))


-- Modals

-- screen width -> (modal width, list of lines)
type ModalMaker = Int -> (Int, [ByteString])

helpModal :: [KeysHelp] -> ModalMaker
helpModal help swd = (wd, map showLine help) where
    wd = commonModalWidth swd
    showLine :: ([Char], ByteString) -> ByteString
    showLine (cs, ps) = toWidth clen cmds <> ps where
        clen = max 4 $ round $ fromIntegral wd * (0.2::Float)
        cmds = P.unwords ("" : map pprIt cs)
        pprIt c = case c of
            '\n' -> "Enter"
            '\f' -> "^L"
            '\\' -> "\\"
            ' '  -> "Space"
            _ -> case charToKey c of
                Curses.KeyUp        -> u"↑"
                Curses.KeyDown      -> u"↓"
                Curses.KeyPPage     -> "PgUp"
                Curses.KeyNPage     -> "PgDn"
                Curses.KeyLeft      -> u"←"
                Curses.KeyRight     -> u"→"
                Curses.KeyEnd       -> "End"
                Curses.KeyHome      -> "Home"
                Curses.KeyBackspace -> "Backspace"
                _ -> u[c]

histModal :: HistDisplay -> ModalMaker
histModal []   _   = let s = "  No history  " in (P.length s, [s])
histModal hist swd = do
    let wd = commonModalWidth swd
        mtlen = maximum $ map (displayWidth . fst) hist
        tlen = min (mtlen + 1) $ wd `div` 3
    (wd, [
        let tstr = toMaxWidth tlen $ P.replicate (tlen - displayWidth time) ' ' <> time
        in mconcat [" ", P.singleton c, " ", tstr, " ", song]
        | (c, (time, (_, song))) <- zip (toList historyKeys ++ repeat ' ') hist ])

exitModal :: ModalMaker
exitModal swd = (wd, ["", padl <> "Exit (y)?", ""]) where
    wd = commonModalWidth swd `min` 19
    padl = P.replicate ((wd - 9) `div` 2) ' '

