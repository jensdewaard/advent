module Common.ListTests (tests) where

import Test.HUnit
import Common.List (swapElems, moveElem, rotate)

swapElemsTest = TestList [
    "swap 0 2 abc = cba" ~: "cba" ~=? swapElems 0 2 "abc",
    "swap 1 3 dabcd = dcbad" ~: "dcbad" ~=? swapElems 1 3 "dabcd",
    "swap 1 1 abc = abc" ~: "abc" ~=? swapElems 1 1 "abc",
    "swap 1 3 dabcd = dcbad" ~: "dcbad" ~=? swapElems 3 1 "dabcd"
    ]

moveElemTest = TestList [
    "move 1 1 abc = abc" ~: "abc" ~=? moveElem 1 1 "abc",
    "move 0 2 abc = bca" ~: "bca" ~=? moveElem 0 2 "abc",
    "move 1 3 acddb = addcb" ~: "addcb" ~=? moveElem 1 3 "acddb",
    "move 1 4 bcdea = bdeac" ~: "bdeac" ~=? moveElem 1 4 "bcdea",
    "move 3 0 bdeac = abdec" ~: "abdec" ~=? moveElem 3 0 "bdeac",
    "move 0 3 abdec = bdeac" ~: "bdeac" ~=? moveElem 0 3 "abdec",
    "move 3 0 bdeac = badec" ~: "badec" ~=? moveElem 3 1 "bdeac"
    ]

rotateTest = TestList [
    "rotate 1 abcd = bcda" ~: "bcda" ~=? rotate 1 "abcd",
    "rotate -1 abcd = dabc" ~: "dabc" ~=? rotate (-1) "abcd",
    "rotate 1 abcde = bcdea" ~: "bcdea" ~=? rotate 1 "abcde"
    ]

tests :: Test
tests = TestList [
    swapElemsTest,
    moveElemTest,
    rotateTest
    ]
