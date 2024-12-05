module Y2024Tests.Day05 (tests) where

import Test.HUnit
import Challenges.Y2024.Day05 (compareUI, uiLE, PageOrdering(..), UpdateItem(..))
import Data.List (sortBy)

orders = [PageOrdering (UpdateItem 75, UpdateItem 53), PageOrdering ( UpdateItem 97, UpdateItem 75), PageOrdering ( UpdateItem 75, UpdateItem  47)]

testSort1 = "" ~: [UpdateItem 97, UpdateItem 75, UpdateItem 47, UpdateItem 61, UpdateItem 53]
    ~=? sortBy (compareUI orders) [UpdateItem 75,UpdateItem 97, UpdateItem 47,UpdateItem 61, UpdateItem 53] 

testSort2 = "" ~: [UpdateItem 97, UpdateItem 75, UpdateItem 47, UpdateItem 61, UpdateItem 53]
    ~=? sortBy (compareUI orders) [UpdateItem 47,UpdateItem 75, UpdateItem 61,UpdateItem 97, UpdateItem 53] 

testLE1 = "" ~: True ~=? uiLE orders (UpdateItem 97) (UpdateItem 75)
testLE2 = "" ~: True ~=? uiLE orders (UpdateItem 75) (UpdateItem 53)
testLE3 = "" ~: False ~=? uiLE orders (UpdateItem 61) (UpdateItem 53)
testLE4 = "" ~: False ~=? uiLE orders (UpdateItem 75) (UpdateItem 97)

tests :: Test
tests = TestList [
    testLE1,
    testLE2,
    testLE3,
    testLE4,
    testSort1,
    testSort2
    ]