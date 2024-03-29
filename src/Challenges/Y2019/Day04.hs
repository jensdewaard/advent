module Challenges.Y2019.Day04 (solutionA, solutionB, isValid, hasTwoButNotThree) where
import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec
import Common.Parsing (int)
import Common.Prelude

solutionA :: String -> String
solutionA = solve parseInput (length . filter isValid)

solutionB :: String -> String
solutionB = solve parseInput (length . filter isValid')

parseInput :: Parser [Int]
parseInput = do
    l <- int
    _ <- char '-'
    h <- int
    return [l..h]

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