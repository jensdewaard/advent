module Challenges.Y2018.Day01 (solutionA, solutionB) where
import Common.Prelude (solve)
import Text.ParserCombinators.Parsec (Parser, newline, sepEndBy1, char, optional)
import Control.Arrow ((>>>))
import Common.Parsing ( int )
import qualified Data.Set as S

solutionA :: String -> String
solutionA = solve parser sum
solutionB :: String -> String
solutionB = solve parser (cycle >>> scanl (+) 0 >>> firstRepeating)

firstRepeating :: [Int] -> Int
firstRepeating = go S.empty where
    go _ [] = undefined
    go seen (x:xs) = if x `S.member` seen then x else go (S.insert x seen) xs

parser :: Parser [Int]
parser = (optional (char '+') >> int) `sepEndBy1` newline