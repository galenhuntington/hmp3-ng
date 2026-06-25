module ElementsSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import System.Clock (TimeSpec(..))

import Elements (showDuration)

tests :: TestTree
tests = testGroup "Elements"
    [ testGroup "showDuration (secs=False)"
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
    , testGroup "showDuration (secs=True)"
        [ testCase "thirty seconds"
            $ showDuration True (t 30)      @?= "30s"
        , testCase "one minute exactly appends 00s"
            $ showDuration True (t 60)      @?= "1m00s"
        , testCase "one minute thirty seconds"
            $ showDuration True (t 90)      @?= "1m30s"
        , testCase "one hour one minute one second"
            $ showDuration True (t 3661)    @?= "1h01m01s"
        ]
    ]

-- Build a TimeSpec from a whole number of seconds.
t :: Integer -> TimeSpec
t s = TimeSpec (fromInteger s) 0

