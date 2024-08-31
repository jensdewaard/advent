module Common.MathTests (tests) where

import Test.HUnit
import Common.Math (crt, mmi)

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
        testMMI
    ]