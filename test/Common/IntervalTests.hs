{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Common.IntervalTests (tests) where

import Test.HUnit
import Common.Interval

ivs :: [Interval Int]
ivs = [
    fromPair (0, 5),
    fromPair (1,1),
    fromPair (-10, 10),
    fromPair (-5, -10)
    ]

testLowerBoundIsBelowUpperBound :: Test
testLowerBoundIsBelowUpperBound = TestLabel "lb i <= ub i" $ TestList $ map (\i -> True ~=? lb i <= ub i) ivs

tests :: Test
tests = TestList [
    testLowerBoundIsBelowUpperBound,
    "the length of the empty interval is 0" ~: Common.Interval.length @Int Empty ~?= 0,
    "the length of a singleton is 1" ~: Common.Interval.length (singleton 5) ~?= 1,
    "[2,4] and [9, 10] are distinct" ~: distinct (fromPair (2,4)) (fromPair (9,10)) ~?= True,
    "the union of (1,4) and (2, 9) = (1,9)" ~: fromPair (1,4) `union` fromPair (2,9) ~?= fromPair (1,9),
    "the union of (4,7) and (1, 9) = (1,9)" ~: fromPair (4,7) `union` fromPair (1,9) ~?= fromPair (1,9)
    ]