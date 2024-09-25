module Y2016Tests.Day21 (tests) where

import Test.HUnit
import Common.List (rotate, swapElems)
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Challenges.Y2016.Day21 (Perm (Perm, PermF), runPerm, invert)

assocTests :: Test
assocTests = let
    p1 = Perm [2,3,0,1]
    p2 = Perm [0,1,3,2]
    p3 = Perm [3,0,1,2]
    p4 = PermF $ \s -> let
                        n = fromJust $ elemIndex 3 s
                        n' = if n >= 4 then n + 2 else n + 1
                        in rotate (negate n') [0..3]
    p5 = PermF $ \s -> let
        x' = fromJust $ elemIndex 2 s
        y' = fromJust $ elemIndex 1 s
                      in swapElems x' y' s
    s = [1..]
    in TestList [
    "p1 <> (p2 <> p3) == (p1 <> p2) <> p3" ~: runPerm ((p1 <> p2) <> p3) s ~=? runPerm (p1 <> (p2 <> p3)) s,
    "p1 <> (p4 <> p3) == (p1 <> p4) <> p3" ~: runPerm ((p1 <> p4) <> p3) s ~=? runPerm (p1 <> (p4 <> p3)) s,
    "p4 <> (p1 <> p4) == (p4 <> p1) <> p4" ~: runPerm ((p4 <> p1) <> p4) s ~=? runPerm (p4 <> (p1 <> p4)) s,
    "p4 <> (p1 <> p5) == (p4 <> p1) <> p5" ~: runPerm ((p4 <> p1) <> p5) s ~=? runPerm (p4 <> (p1 <> p5)) s
    ]
tests :: Test
tests = TestList [
    assocTests
    ]
