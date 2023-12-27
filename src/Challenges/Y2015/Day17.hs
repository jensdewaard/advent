module Challenges.Y2015.Day17 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Common.Prelude
import Data.List (subsequences)

solutionA :: String -> String
solutionA = solve parser (length . filter (\s -> sum s == 150) . subsequences)
solutionB :: String -> String
solutionB = solve parser (length . allShortest . filter (\s -> sum s == 150) . subsequences)

allShortest :: [[a]] -> [[a]]
allShortest as = let ml = minimum $ map length as in
    filter (\s -> length s == ml) as

parser :: Parser [Int]
parser = (read <$> many1 digit) `sepEndBy` newline