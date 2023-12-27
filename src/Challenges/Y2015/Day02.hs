module Challenges.Y2015.Day02 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Data.List (sort)
import Parsing (int)
import Common.Prelude

solutionA :: String -> String
solutionA = solve parseInput (sum . map requiredPaper)
solutionB :: String -> String
solutionB = solve parseInput (sum . map requiredRibbon)

requiredPaper :: (Int, Int, Int) -> Int
requiredPaper (w,h,l) = 2*(w * h
    + w * l
    + l * h) + minimum [w * h, w * l, l * h]

requiredRibbon :: (Int, Int, Int) -> Int
requiredRibbon (w,h,l) = (w * h * l) +
    2 * sum (take 2 $ sort [w, l, h])

parseInput :: Parser [(Int, Int, Int)]
parseInput = sepEndBy present newline

present :: Parser (Int, Int, Int)
present = do
    l <- int
    _ <- string "x"
    w <- int
    _ <- string "x"
    h <- int
    return (w,h,l)
