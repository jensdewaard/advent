module Y2019Tests.Day01 (tests) where

import Test.HUnit
import Challenges.Y2019.Day01 (fuelRequired, fuelRequiredR)

test12 :: Test
test12 = "12 = 2" ~: fuelRequired 12 ~=? 2

test14R :: Test
test14R = "14R = 2" ~: 2 ~=? fuelRequiredR 14 

test14 :: Test
test14 = "14 = 2" ~: 2 ~=? fuelRequired 14

test1969 :: Test
test1969 = "1969 = 654" ~: fuelRequired 1969 ~=? 654

test1969R :: Test
test1969R = "1969R = 966" ~: fuelRequiredR 1969 ~=? 966

test100756 :: Test
test100756 = "100756 = 33583" ~: fuelRequired 100756 ~=? 33583

test100756R :: Test
test100756R = "100756R = 33583" ~: fuelRequiredR 100756 ~=? 50346

tests :: Test
tests = TestList [
    TestLabel "12" test12,
    TestLabel "14" test14,
    TestLabel "1969" test1969,
    TestLabel "100756" test100756,
    TestLabel "1969R" test1969R,
    TestLabel "14R" test14R,
    TestLabel "100756" test100756R
    ]