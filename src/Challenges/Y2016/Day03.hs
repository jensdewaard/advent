module Challenges.Y2016.Day03 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec
import Common.Parsing (int)

solutionA :: String -> String
solutionA = solve parser (length . filter possible)
solutionB :: String -> String
solutionB = solve parser (length . filter possible . flipInput)

flipInput :: [(Int, Int, Int)] -> [(Int, Int, Int)]
flipInput [] = []
flipInput ((a,b,c):(d,e,f):(g,h,i):ts) = (a,d,g) : (b,e,h) : (c,f,i) : flipInput ts
flipInput _ = error "number of rows not a multiple of three"

possible :: (Int, Int, Int) -> Bool
possible (a,b,c) = a + b > c && b + c > a && a + c > b

parser :: Parser [(Int, Int, Int)]
parser = triangle `sepEndBy` newline where
    triangle = do
        _ <- optional spaces
        a <- int
        _ <- spaces
        b <- int
        _ <- spaces
        c <- int
        return (a,b,c)