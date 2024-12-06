{-# LANGUAGE  TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Common.ListTests (runTests) where

import Common.List (swapElems, differences)
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck ((==>), Property)

prop_swapelems_refl :: Int -> String -> Bool
prop_swapelems_refl n s = swapElems n n s == swapElems n n s

prop_swapelems_comm :: Int -> Int -> String -> Bool
prop_swapelems_comm n m s = swapElems n m s == swapElems m n s

prop_differences_oneshorter :: (Eq a, Num a) => [a] -> Property
prop_differences_oneshorter ns = ns /= [] ==> length ns == length (differences ns) + 1

prop_differences_scanl_reverses :: (Eq a, Num a) => [a] -> Property
prop_differences_scanl_reverses ns = ns /= [] ==> scanl (+) (head ns) (differences ns) == ns

prop_differences_empty :: Bool
prop_differences_empty = null (differences [])

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll

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
