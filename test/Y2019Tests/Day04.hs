module Y2019Tests.Day04 (tests) where

import Test.HUnit
import Challenges.Y2019.Day04 (isValid, hasThree)

test111111 = "111111 is valid" ~: isValid 111111 ~=? True
test223450 = "223450 is not valid" ~: False ~=? isValid 223450 
test123789 = "123789 is not valid" ~: False ~=? isValid 123789 

testHasThree1 = "1122333" ~: True ~=? hasThree "1122333"
testHasThree2 = "112233" ~: False ~=? hasThree "112233"
testHasThree3 = "1112233" ~: True ~=? hasThree "1112233"

tests :: Test
tests = TestList [
    TestLabel "111111 is valid" test111111,
    TestLabel "223450 is not valid" test223450,
    TestLabel "123789 is not valid" test123789,
    TestLabel "1122333 hasThree" testHasThree1,
    TestLabel "112233 not hasThree" testHasThree2,
    TestLabel "112233 not hasThree" testHasThree3
    ]