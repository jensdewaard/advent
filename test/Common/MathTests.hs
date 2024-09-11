module Common.MathTests (tests) where

import Test.HUnit
import Common.Math (crt, mmi, josephus, biggestPot)

testCrt1 :: Test
testCrt1 = let
    desc = "x = 2 (mod 6), x = 1 (mod 7) -> 8"
    eqs = [(2,6),(1,7)]
    in desc ~: 8 ~=? crt eqs

testCrt2 :: Test
testCrt2 = let 
        desc = "x = 3 (mod 5), x = 2 (mod 7), x = 5 (mod 9) -> 23"
        eqs = [(3,5),(2,7),(5,9)]
    in desc ~: 23 ~=? crt eqs

testMMI :: Test
testMMI = "3x = 1 mod 11 -> x = 4" ~: 4 ~=? mmi 3 11

tests :: Test
tests = TestList [
        testCrt1,
        testCrt2,
        testMMI,
        biggestPotTest,
        josephusTests
    ]

biggestPotTest :: Test
biggestPotTest = TestList [
    TestCase $ 2 @=? biggestPot 2,
    TestCase $ 4 @=? biggestPot 5,
    TestCase $ 8 @=? biggestPot 8,
    TestCase $ 8 @=? biggestPot 10
    ]

josephusTests :: Test
josephusTests = TestList [
    TestCase $ 1 @=? josephus 1,
    TestCase $ 1 @=? josephus 2,
    TestCase $ 3 @=? josephus 3,
    TestCase $ 1 @=? josephus 4,
    TestCase $ 3 @=? josephus 5,
    TestCase $ 5 @=? josephus 6,
    TestCase $ 7 @=? josephus 7,
    TestCase $ 1 @=? josephus 8,
    TestCase $ 3 @=? josephus 9,
    TestCase $ 5 @=? josephus 10,
    TestCase $ 7 @=? josephus 11,
    TestCase $ 9 @=? josephus 12,
    TestCase $ 11 @=? josephus 13,
    TestCase $ 13 @=? josephus 14
    ]
