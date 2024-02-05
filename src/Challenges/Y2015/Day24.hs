module Challenges.Y2015.Day24 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Common.Prelude
import Common.Parsing (int)
import Data.List (subsequences)

solutionA :: String -> String
solutionA = solve parser (group 3)
solutionB :: String -> String
solutionB = solve parser (group 4)

group :: Int -> [Int] -> Int
group n weights = let
      groupSize = sum weights `div` n
      combos = filter (\l -> sum l == groupSize) $ subsequences weights
      minLength = minimum (map length combos)
      qes = filter (>0) $ map product $ filter (\l -> length l == minLength) combos
      in
      minimum qes

parser :: Parser [Int]
parser = int `sepEndBy1` newline
