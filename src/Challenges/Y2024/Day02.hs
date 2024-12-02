module Challenges.Y2024.Day02 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import Common.Parsing (int)
import Control.Arrow ((>>>))
import Common.List (monotone, differences, subsets)

solutionA :: String -> String
solutionA = solve parser (map safe >>> filter id >>> length)
solutionB :: String -> String
solutionB = solve parser (map safe' >>> filter id >>> length)

safe :: [Int] -> Bool
safe ns = safeTolerance ns && monotone ns

safe' :: [Int] -> Bool
safe' ns = safe ns || any safe (subsetsN (length ns - 1) ns)

subsetsN :: Int -> [Int] -> [[Int]]
subsetsN n = filter ((==n).length) . subsets

safeTolerance :: [Int] -> Bool
safeTolerance = all ((\d -> d >= 1 && d <= 3) . abs) . differences

parser :: Parser [[Int]]
parser = int `sepBy1` char ' ' `sepEndBy1` newline