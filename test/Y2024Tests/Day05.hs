module Y2024Tests.Day05 (tests) where

import Test.HUnit
import Challenges.Y2024.Day05 (PageOrdering(..), UpdateItem(..))
import Data.List (sort)

orders = [PageOrdering (75, 53), PageOrdering (97, 75), PageOrdering (75, 47)]

mkItem :: Int -> Int -> UpdateItem
mkItem n = UpdateItem n orders

testSort1 = "" ~: zipWith (\f n -> f n) [mkItem 97, mkItem 75, mkItem 47, mkItem 61, mkItem 53] [1..]
    ~=? sort (zipWith (\f n -> f n ) [mkItem 75,mkItem 97, mkItem 47,mkItem 61, mkItem 53] [1..])

testSort2 = "" ~: zipWith (\f n -> f n) [mkItem 97, mkItem 75, mkItem 47, mkItem 61, mkItem 53] [1..]
    ~=? sort (zipWith (\f n -> f n) [mkItem 47,mkItem 75, mkItem 61,mkItem 97, mkItem 53] [1..])

testLE1 = "" ~: True ~=? mkItem 97 0 <= mkItem 75 1
testLE2 = "" ~: True ~=? mkItem 75 1 <= mkItem 53 0
testLE3 = "" ~: LT ~=? compare (mkItem 61 0) (mkItem 53 1)
testLE4 = "" ~: False ~=? mkItem 75 0 <= mkItem 97 1

tests :: Test
tests = TestList [
    -- testLE1,
    -- testLE2,
    -- testLE3,
    -- testLE4,
    -- testSort1,
    -- testSort2
    ]