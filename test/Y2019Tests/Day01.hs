module Y2019Tests.Day01 (tests) where

import Test.HUnit
import Challenges.Y2019.Day01 (fuelRequired)

test12 :: Test
test12 = "12 = 2" ~: fuelRequired 12 ~=? 2

test14 :: Test
test14 = "14 = 2" ~: fuelRequired 14 ~=? 2

test1969 :: Test
test1969 = "1969 = 654" ~: fuelRequired 1969 ~=? 654

test100756 :: Test
test100756 = "100756 = 33583" ~: fuelRequired 100756 ~=? 33583

tests :: Test
tests = TestList [
    TestLabel "12" test12,
    TestLabel "14" test14,
    TestLabel "1969" test1969,
    TestLabel "100756" test100756
    ]