module Challenges.Y2023.Day09 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Shared (solve, allEqual)
import Parsing (int)

solutionA :: String -> String
solutionA = solve parseInput (sum . map determineNext)
solutionB :: String -> String
solutionB = solve parseInput (sum . map (determineNext . reverse))

determineNext :: [Integer] -> Integer
determineNext ns
    | allEqual ns = head ns
    | otherwise = last ns + determineNext (diff ns)

diff :: [Integer] -> [Integer]
diff [] = []
diff [_] = []
diff (a:b:cs) = (b-a) : diff (b:cs)

parseInput :: Parser [[Integer]]
parseInput = history `sepEndBy1` newline where
    history :: Parser [Integer]
    history = (toInteger <$> int) `sepBy1` char ' '