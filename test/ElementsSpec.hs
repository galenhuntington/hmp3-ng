module ElementsSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.ByteString.Char8 qualified as P
import System.Clock (TimeSpec(..))

import Base
import Text (displayWidth)
import Elements (showDuration, fitLCR, layoutLCR, Fit(..))

tests :: TestTree
tests = testGroup "Elements"
    [ testGroup "showDuration (showSecs=False)"
        [ testCase "under a minute is 0m"
            $ showDuration False (t 30)    @?= "0m"
        , testCase "exactly one minute"
            $ showDuration False (t 60)    @?= "1m"
        , testCase "under an hour"
            $ showDuration False (t 599)   @?= "9m"
        , testCase "exactly one hour"
            $ showDuration False (t 3600)  @?= "1h00m"
        , testCase "one hour, one minute, one second drops seconds"
            $ showDuration False (t 3661)  @?= "1h01m"
        , testCase "exactly one day"
            $ showDuration False (t 86400) @?= "1d00h00m"
        , testCase "day, hour, minute"
            $ showDuration False (t 90060) @?= "1d01h01m"
        ]
    , testGroup "showDuration (showSecs=True)"
        [ testCase "thirty seconds"
            $ showDuration True (t 30)     @?= "30s"
        , testCase "one minute exactly appends 00s"
            $ showDuration True (t 60)     @?= "1m00s"
        , testCase "one minute thirty seconds"
            $ showDuration True (t 90)     @?= "1m30s"
        , testCase "one hour one minute one second"
            $ showDuration True (t 3661)   @?= "1h01m01s"
        ]
    , testCase "Title layout" fitTests
    ]

-- Build a TimeSpec from a whole number of seconds.
t :: Integer -> TimeSpec
t s = TimeSpec (fromInteger s) 0

fitTests :: Assertion
fitTests = sequence_ do
    w <- [1..40]
    lsz <- [0..9]
    csz <- [0,1,4,5,6,8]  -- always 6 currently
    rsz <- [0..9]
    let inp = (lsz, csz, rsz)
    let ans@Fit{..} = fitLCR w inp
    let go as wh = as (wh ++ " " ++ show (w, inp) ++ " -> " ++ show ans)
    pure do
        -- Check several invariants.
        go assertBool "Positive padl" $ padL > 0
        go assertBool "Positive padr" $ padR > 0 || (ctake == 0 && padR == 0)
        go assertBool "Nonnegative ctake" $ ctake >= 0
        go assertEqual "Total width" w $
            padL + padR + ctake + (if wide then lsz + rsz else 0)
        go assertEqual "Show iff possible" wide (lsz + csz + rsz + 2 <= w)
        when (padL > 1 && padR > 1) do
            go assertEqual "Full center if possible" ctake csz
            go assertBool "Centered if possible" $
                let a = padL + (if wide then lsz else 0)
                    b = padR + (if wide then rsz else 0)
                in a == b || a + 1 == b
        let s = layoutLCR w (P.replicate lsz 'x', replicate csz 'x', P.replicate rsz 'x')
        assertEqual ("String width: " ++ show inp ++ " -> " ++ show s) w
            $ displayWidth s

