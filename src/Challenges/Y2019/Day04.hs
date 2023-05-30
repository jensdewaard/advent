module Challenges.Y2019.Day04 where
import Data.Char (digitToInt)

solutionA :: String -> String
solutionA _ = show $ length [x | x <- [172851..675869], isValid x]

-- 889 is wrong
-- 905 is too low
-- 945 is wrong
-- 1449 is wrong
-- 1478 is too high
-- 1185 is wrong
solutionB :: String -> String
solutionB _ = show $ length [x | x <- [172851..675869], isValid' x]

input :: Bool -> IO String
input _ = return ""

isValid' :: Int -> Bool
isValid' i = hasTwoButNotThree i' && isNotDecreasing i' where i' = show i

isValid :: Int -> Bool
isValid i = hasConsecutive i' && isNotDecreasing i' where i' = show i

hasTwoButNotThree :: String -> Bool
hasTwoButNotThree (x : y : z : ss)
    | (x == y) && (y == z) = hasTwoButNotThree (dropWhile (== x) ss)
    | (x == y) && (y /= z) = True
    | x /= y = hasTwoButNotThree (y : z :ss)
hasTwoButNotThree [x , y] = x == y
hasTwoButNotThree _ = False

hasConsecutive :: String -> Bool
hasConsecutive (x : y :  ss) = (x == y) || hasConsecutive (y : ss)
hasConsecutive _ = False

isNotDecreasing :: String -> Bool
isNotDecreasing (x : y : ss) = (y' >= x') && isNotDecreasing (y : ss) where
    x' = digitToInt x
    y' = digitToInt y
isNotDecreasing [_] = True
isNotDecreasing [] = True