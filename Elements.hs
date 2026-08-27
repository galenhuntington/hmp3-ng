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

import System.Clock
import UI.HSCurses.Curses qualified as Curses


package :: String
package = "hmp3-ng"

fullVersion :: String
fullVersion  = package ++ " v" ++ showVersion version

-- | Version info
pVersion :: SText
pVersion = fromString fullVersion

commonModalWidth :: Int -> Int
commonModalWidth w = max (min w 3) $ round $ fromIntegral w * (0.8::Float)

showClock :: Fixed E2 -> SText
showClock t =
    let m, si, sd :: Int
        (m, s) = t `divMod'` 60
        si     = floor s
        sd     = floor (s*10) `mod` 10
    in fromString $ printf "%d:%02d.%d" m si sd

-- | Human-friendly duration, with a flag to include seconds.
showDuration :: Bool -> TimeSpec -> SText
showDuration showSecs tm
    | ms == 0 && showSecs
              = go ""
    | hs == 0 = go $ printf "%dm" m
    | d == 0  = go $ printf "%dh%02dm" h m
    | True    = go $ printf "%dd%02dh%02dm" d h m
  where
    go      = fromString . ss
    (ms, s) = sec tm `quotRem` 60
    (hs, m) = ms `quotRem` 60
    (d, h)  = hs `quotRem` 24
    ss      =
        if showSecs then (<> printf (if ms > 0 then "%02ds" else "%ds") s) else id

-- | The time used and time left
pTimes :: Int -> Maybe Frame -> SText
pTimes w clock
    | w - 4 < byteLength elapsed = ""
    | True                       =
        mconcat $ ["  ", elapsed] ++ [gap <> "-" <> left | distance > 0]
  where
    elapsed  = showClock (maybe 0 (.elapsed) clock)
    left     = maybe "?:??.?" (showClock . (.left)) clock
    gap      = spaces distance
    distance = w - 5 - byteLength elapsed - byteLength left

-- | Progress out of total
progress :: Int -> Maybe Frame -> Int
progress width = maybe 0 \fr ->
    let total    = curr + toRational fr.left - ε
        curr     = toRational fr.elapsed
        ε        = 1 / 200
    in ceiling (curr * fromIntegral (width - 1) / total)

data Fit = Fit { wide :: !Bool, padL :: !Int, padR :: !Int, ctake :: !Int }
    deriving stock Show

-- | Given a width and size of left, center, and right elements, determine
-- whether left and right can fit, padding between, and amount of center to show
fitLCR :: Int -> (Int, Int, Int) -> Fit
fitLCR w (lsz, csz, rsz) = if
    | gap >= 2   -> let gapl = 1 `max` ((side - lsz) `min` (gap - 1))
                    in Fit True gapl (gap - gapl) csz
    | w-2 >= csz -> Fit False side (sides - side) csz
    | w > 1      -> Fit False 1 1 (w-2)
    | True       -> Fit False w 0 0
  where
    sides = w - csz
    side = sides `div` 2
    gap  = sides - lsz - rsz

layoutLCR :: Int -> (SText, String, SText) -> SText
layoutLCR w (left, centerS, right) = mconcat [
    if fit.wide then left else "",
    spaces fit.padL,
    fromString $ take fit.ctake centerS,
    spaces fit.padR,
    if fit.wide then right else ""
    ]
  where
    fit = fitLCR w (byteLength left, length centerS, byteLength right)


-- Modals

-- screen width -> (modal width, list of lines)
type ModalMaker = Int -> (Int, [SText])

helpModal :: [KeysHelp] -> ModalMaker
helpModal help swd = (wd, map showLine help) where
    wd = commonModalWidth swd
    showLine :: ([Char], SText) -> SText
    showLine (cs, ps) = toWidth clen cmds <> ps where
        clen = max 4 $ round $ fromIntegral wd * (0.2::Float)
        cmds = mconcat $ intersperse " " $ "" : map pprIt cs
        pprIt c = case c of
            '\n' -> "Enter"
            '\f' -> "^L"
            '\\' -> "\\"
            ' '  -> "Space"
            _ -> case charToKey c of
                Curses.KeyUp        -> "↑"
                Curses.KeyDown      -> "↓"
                Curses.KeyPPage     -> "PgUp"
                Curses.KeyNPage     -> "PgDn"
                Curses.KeyLeft      -> "←"
                Curses.KeyRight     -> "→"
                Curses.KeyEnd       -> "End"
                Curses.KeyHome      -> "Home"
                Curses.KeyBackspace -> "Backspace"
                _ -> fromString [c]

histModal :: HistDisplay -> ModalMaker
histModal []   _   = let s = "  No history  " in (byteLength s, [s])
histModal hist swd = do
    let wd = commonModalWidth swd
        mtlen = maximum $ map (displayWidth . fst) hist
        tlen = min (mtlen + 1) $ wd `div` 3
    (wd, [
        let tstr = toMaxWidth tlen $ spaces (tlen - displayWidth time) <> time
        in mconcat [" ", fromString [c], " ", tstr, " ", song]
        | (c, (time, (_, song))) <- zip (toList historyKeys ++ repeat ' ') hist ])

exitModal :: ModalMaker
exitModal swd = (wd, ["", padl <> "Exit (y)?", ""]) where
    wd = commonModalWidth swd `min` 19
    padl = spaces ((wd - 9) `div` 2)

