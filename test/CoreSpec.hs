module CoreSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import System.Clock (TimeSpec(..))

import Core (showTimeDiff_)

tests :: TestTree
tests = testGroup "Core"
    [ testGroup "showTimeDiff_ (secs=False)"
        [ testCase "under a minute is 0m"
            $ showTimeDiff_ False (t 0) (t 30)    @?= "0m"
        , testCase "exactly one minute"
            $ showTimeDiff_ False (t 0) (t 60)    @?= "1m"
        , testCase "under an hour"
            $ showTimeDiff_ False (t 0) (t 599)   @?= "9m"
        , testCase "exactly one hour"
            $ showTimeDiff_ False (t 0) (t 3600)  @?= "1h00m"
        , testCase "one hour, one minute, one second drops seconds"
            $ showTimeDiff_ False (t 0) (t 3661)  @?= "1h01m"
        , testCase "exactly one day"
            $ showTimeDiff_ False (t 0) (t 86400) @?= "1d00h00m"
        , testCase "day, hour, minute"
            $ showTimeDiff_ False (t 0) (t 90060) @?= "1d01h01m"
        ]
    , testGroup "showTimeDiff_ (secs=True)"
        [ testCase "thirty seconds"
            $ showTimeDiff_ True (t 0) (t 30)      @?= "30s"
        , testCase "one minute exactly appends 00s"
            $ showTimeDiff_ True (t 0) (t 60)      @?= "1m00s"
        , testCase "one minute thirty seconds"
            $ showTimeDiff_ True (t 0) (t 90)      @?= "1m30s"
        , testCase "one hour one minute one second"
            $ showTimeDiff_ True (t 0) (t 3661)    @?= "1h01m01s"
        , testCase "diff is taken from monotonic delta, not absolute values"
            $ showTimeDiff_ True (t 1000) (t 1090) @?= "1m30s"
        ]
    ]

-- Build a TimeSpec from a whole number of seconds.
t :: Integer -> TimeSpec
t s = TimeSpec (fromInteger s) 0
