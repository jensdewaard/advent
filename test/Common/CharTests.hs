module Common.CharTests (tests) where

import Test.HUnit
import Common.Char (shift)

testShifta :: Test
testShifta = "shift 1 a = b" ~: 'b' ~=? shift 1 'a'

testShiftz :: Test
testShiftz = "shift 1 z = a" ~: 'a' ~=? shift 1 'z'

testShiftWraps :: Test
testShiftWraps = "shift 53 d = e" ~: 'e' ~=? shift 53 'd' 

testShiftCapitalA :: Test
testShiftCapitalA = "shift 3 A = D" ~: 'D' ~=? shift 3 'A'

testShiftCapitalZ :: Test
testShiftCapitalZ = "shift 9 Z = I" ~: 'I' ~=? shift 9 'Z'

tests :: Test
tests = TestList [
    testShifta, testShiftWraps, testShiftz,
    testShiftCapitalA, testShiftCapitalZ
    ]