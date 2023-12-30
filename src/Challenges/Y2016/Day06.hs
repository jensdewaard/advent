module Challenges.Y2016.Day06 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import Common.List (occur)
import Data.List (transpose, minimumBy, maximumBy)
import Data.Ord (comparing)

solutionA :: String -> String
solutionA = solve parser (map (fst . maximumBy (comparing snd) . occur) . transpose)
solutionB :: String -> String
solutionB = solve parser (map (fst . minimumBy (comparing snd) . occur) . transpose)

parser :: Parser [String]
parser = many1 letter `sepEndBy` newline