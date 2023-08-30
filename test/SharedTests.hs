module SharedTests (tests) where

import Shared (rectFromTo)
import Test.HUnit

testRectFromTo :: Test
testRectFromTo = "1" ~: [(0,0), (0,1), (1,0), (1,1)] ~=? (rectFromTo (0,0) (1,1)) 

testRectFromTo_Rectangle :: Test
testRectFromTo_Rectangle = "1" ~: [(0,0), (0,1), (0, 2), (1,0), (1,1), (1, 2)] ~=? (rectFromTo (0,0) (1,2)) 

tests :: Test
tests = TestList [
    TestLabel "small square" testRectFromTo,
    TestLabel "rectangle" testRectFromTo_Rectangle
    ]