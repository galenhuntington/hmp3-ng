module ElementsSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import System.Clock (TimeSpec(..))

import Base
import Elements (showDuration, fitLCR)

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
            $ showDuration True (t 30)      @?= "30s"
        , testCase "one minute exactly appends 00s"
            $ showDuration True (t 60)      @?= "1m00s"
        , testCase "one minute thirty seconds"
            $ showDuration True (t 90)      @?= "1m30s"
        , testCase "one hour one minute one second"
            $ showDuration True (t 3661)    @?= "1h01m01s"
        ]
    , testCase "fitLCR" fitTests
    ]

-- Build a TimeSpec from a whole number of seconds.
t :: Integer -> TimeSpec
t s = TimeSpec (fromInteger s) 0

fitTests :: Assertion
fitTests = sequence_ do
    w <- [1..50]
    lsz <- [0..20]
    csz <- [4..8]
    rsz <- [0..20]
    let inp = (lsz, csz, rsz)
    let ans@(lr, padl, padr, ctake) = fitLCR w inp
    let go as wh = as (wh ++ " " ++ show (w, inp) ++ " -> " ++ show ans)
    pure do
        -- Check several invariants.
        go assertBool "Positive padl" $ padl > 0
        go assertBool "Positive padr" $ padr > 0 || (ctake == 0 && padr == 0)
        go assertBool "Nonnegative ctake" $ ctake >= 0
        go assertEqual "Total width" w $
            padl + padr + ctake + (if lr then lsz + rsz else 0)
        go assertEqual "Show iff possible" lr (lsz + csz + rsz + 2 <= w)
        when (padl > 1 && padr > 1) do
            go assertEqual "Full center if possible" ctake csz
            go assertBool "Centered if possible" $
                let a = padl + (if lr then lsz else 0)
                    b = padr + (if lr then rsz else 0)
                in a == b || a + 1 == b

