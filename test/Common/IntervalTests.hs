{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Common.IntervalTests (runTests) where

import Test.HUnit
import Common.Interval
import Test.QuickCheck.All 
import Test.QuickCheck 
import Test.QuickCheck.Classes
import Data.Proxy (Proxy (Proxy))

ivs :: [Interval Int]
ivs = [
    fromPair (0, 5),
    fromPair (1,1),
    fromPair (-10, 10),
    fromPair (-5, -10)
    ]

testLowerBoundIsBelowUpperBound :: Test
testLowerBoundIsBelowUpperBound = TestLabel "lb i <= ub i" $ TestList $ map (\i -> True ~=? lb i <= ub i) ivs

removeTest1 :: Test
removeTest1 = let
        expected = [ fromPair (1,1), fromPair (4,5) ]
        actual = fromPair (1,5) `remove` fromPair (2,3)
    in
    "(1,5) remove (2,3) = [(1,1), (4,5)]" ~: expected ~=? actual

removeTest2 :: Test
removeTest2 = let
        expected = [ fromPair (6,9) ]
        actual = fromPair (3,9) `remove` fromPair (1,5)
    in
    "(3,9) remove (1,5) = [(6,9)]" ~: expected ~=? actual

removeTest3 :: Test
removeTest3 = let
        expected = [ fromPair (3,4) ]
        actual = fromPair (3,9) `remove` fromPair (5,12)
    in
    "(3,9) remove (5,12) = [(3,4)]" ~: expected ~=? actual

removeTest4 :: Test
removeTest4 = let
        expected = [ fromPair (1,9) ]
        actual = fromPair (1,9) `remove` fromPair (11,22)
    in
    "(1,9) remove (11,22) = [(1,9)]" ~: expected ~=? actual

removeTest5 :: Test
removeTest5 = let
        expected = [ ]
        actual = fromPair (1,9) `remove` fromPair (-5,20)
    in
    "(1,9) remove (-5,20) = [ Empty ]" ~: expected ~=? actual

removeTest6 :: Test
removeTest6 = let
        expected = [ ]
        actual = fromPair (1,1) `remove` fromPair (1,1)
    in
    "(1,1) remove (1,1) = [ ]" ~: expected ~=? actual

removeAll1 :: Test
removeAll1 = let
    expected = [ fromPair (3,4), fromPair (9,9)]
    actual = [ fromPair (1,4), fromPair (9,9)] `removeAll` fromPair (0,2)
             in "[(1,4),(9,9) removeAll (0,2) = [(3,4), (9,9)]]" ~: expected ~=? actual

removeAll2 :: Test
removeAll2 = let
    expected = [ ]
    actual = [ fromPair (0,0) ] `removeAll` fromPair (0,0)
             in "[(0,0) removeAll (0,0) = []" ~: expected ~=? actual

tests :: Test
tests = TestList [
    removeTest1,
    removeTest2,
    removeTest3,
    removeTest4,
    removeTest5,
    removeTest6,
    removeAll1,
    removeAll2,
    testLowerBoundIsBelowUpperBound,
    "the length of the empty interval is 0" ~: Common.Interval.length @Int Empty ~?= 0,
    "the length of a singleton is 1" ~: Common.Interval.length (singleton 5) ~?= 1,
    "[2,4] and [9, 10] are distinct" ~: distinct (fromPair (2,4)) (fromPair (9,10)) ~?= True,
    "the union of (1,4) and (2, 9) = (1,9)" ~: fromPair (1,4) `union` fromPair (2,9) ~?= fromPair (1,9),
    "the union of (4,7) and (1, 9) = (1,9)" ~: fromPair (4,7) `union` fromPair (1,9) ~?= fromPair (1,9)
    ]

instance Arbitrary a => Arbitrary (Interval a) where
  arbitrary = applyArbitrary2 Interval

intervalInt :: Proxy (Interval Int)
intervalInt = Proxy

laws :: (Ord a, Monoid a, Arbitrary a, Show a) => Proxy a -> [Laws]
laws p = [eqLaws p, ordLaws p, semigroupLaws p, semigroupMonoidLaws p, monoidLaws p]

--------------------------
return []
runTests :: IO Bool
runTests = do
    lawsCheckMany [("Interval Int", laws intervalInt)]
    $quickCheckAll