module Challenges.Y2016.Day20 (solutionA, solutionB) where
import Common.Prelude
import Common.Parsing (int)
import Common.Interval
import Text.ParserCombinators.Parsec

solutionA :: String -> String
solutionA = solve parser (minimum . solveA)
solutionB :: String -> String
solutionB = solve parser (Prelude.length . filter (<= 4294967295) . solveA)

solveA :: [Interval Int] -> [Int]
solveA js = let
    candidates = map ((+1) . ub) (simplify js)
    f :: Int -> Bool
    f x = any (x `isIn`) js
            in filter (not . f) candidates

parser :: Parser [Interval Int]
parser = interval `sepEndBy` newline where
    interval = do
        lowerBound <- int
        _ <- string "-"
        Interval lowerBound <$> int
