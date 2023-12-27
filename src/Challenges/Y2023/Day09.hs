module Challenges.Y2023.Day09 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Common.Prelude
import Common.Parsing (int)

solutionA :: String -> String
solutionA = solve parseInput (sum . map determineNext)
solutionB :: String -> String
solutionB = solve parseInput (sum . map (determineNext . reverse))

determineNext :: (Eq a, Num a) => [a] -> a
determineNext [] = error "cannot determine next element of empty sequence"
determineNext (n:ns) = if all (==n) ns
    then n
    else last ns + determineNext (diff ns)

diff :: Num a => [a] -> [a]
diff ns = zipWith (-) ns (tail ns)

parseInput :: Parser [[Integer]]
parseInput = history `sepEndBy1` newline where
    history :: Parser [Integer]
    history = (toInteger <$> int) `sepBy1` char ' '