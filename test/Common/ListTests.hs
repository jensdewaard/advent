module Common.ListTests (tests, prop_swapelems_inv, prop_swapelems_comm) where

import Test.QuickCheck
    ( chooseAny,
      suchThat,
      forAll,
      Arbitrary(arbitrary),
      Gen,
      Property,
      Result(reason) )
import Common.List (swapElems, moveElem, rotate)
import Control.Arrow ((>>>))
import Test.QuickCheck.Property (succeeded, failed)

-- swapElemsTest = TestList [
--     "swap 0 2 abc = cba" ~: "cba" ~=? swapElems 0 2 "abc",
--     "swap 1 3 dabcd = dcbad" ~: "dcbad" ~=? swapElems 1 3 "dabcd",
--     "swap 1 1 abc = abc" ~: "abc" ~=? swapElems 1 1 "abc",
--     "swap 1 3 dabcd = dcbad" ~: "dcbad" ~=? swapElems 3 1 "dabcd"
--     ]

prop_swapelems_inv :: [Char] -> Property
prop_swapelems_inv s
    = forAll (chooseAny `suchThat` (\x -> 0 <= x && x <= 10)) $ \m ->
      forAll (chooseAny `suchThat` (\x -> 0 <= x && x < m)) $ \n ->
        swapElems n m (swapElems m n s) == s

prop_swapelems_refl :: Int -> String -> Bool
prop_swapelems_refl n s = swapElems n n s == swapElems n n s

prop_swapelems_comm :: Int -> Int -> String -> Bool
prop_swapelems_comm n m s = swapElems n m s == swapElems m n s

-- moveElemTest = TestList [
--     "move 1 1 abc = abc" ~: "abc" ~=? moveElem 1 1 "abc",
--     "move 0 2 abc = bca" ~: "bca" ~=? moveElem 0 2 "abc",
--     "move 1 3 acddb = addcb" ~: "addcb" ~=? moveElem 1 3 "acddb",
--     "move 1 4 bcdea = bdeac" ~: "bdeac" ~=? moveElem 1 4 "bcdea",
--     "move 3 0 bdeac = abdec" ~: "abdec" ~=? moveElem 3 0 "bdeac",
--     "move 0 3 abdec = bdeac" ~: "bdeac" ~=? moveElem 0 3 "abdec",
--     "move 3 0 bdeac = badec" ~: "badec" ~=? moveElem 3 1 "bdeac"
--     ]

-- rotateTest = TestList [
--     "rotate 1 abcd = bcda" ~: "bcda" ~=? rotate 1 "abcd",
--     "rotate -1 abcd = dabc" ~: "dabc" ~=? rotate (-1) "abcd",
--     "rotate 1 abcde = bcdea" ~: "bcdea" ~=? rotate 1 "abcde"
--     ]

-- tests :: Test
-- tests = TestList [
--     swapElemsTest,
--     moveElemTest,
--     rotateTest
--     ]
tests = [prop_swapelems_inv]
