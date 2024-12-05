module Common.PosetTests (tests) where

import Common.Poset (Poset((<=)), coveringRelation)
import Test.HUnit
import Prelude hiding ((<=))

data V = A | B | C deriving Eq
instance Poset V where
    (<=) A A = True
    (<=) A B = True
    (<=) B B = True
    (<=) A C = True
    (<=) B C = True
    (<=) C C = True
    (<=) _ _ = False

(⋖) :: V -> V -> Bool
(⋖) = flip $ coveringRelation [A,B,C] (<=)
infix 6 ⋖

testCoveringRelation :: Test
testCoveringRelation = TestList [
    "B covers A" ~: True  ~=? A ⋖ B,
    "C covers B" ~: True  ~=? B ⋖ C,
    "C covers A" ~: False  ~=? A ⋖ C
    ]

tests :: Test
tests = TestList [
    testCoveringRelation
    ]