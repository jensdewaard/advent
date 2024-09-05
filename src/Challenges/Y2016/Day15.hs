module Challenges.Y2016.Day15 (solutionA, solutionB) where
import Common.Prelude
import Common.Parsing
import Common.Math (crt)
import Text.ParserCombinators.Parsec

solutionA :: String -> String
solutionA = solve parser (crt . map s)
solutionB :: String -> String
solutionB = solve parser (crt . map s . (++ [(7,11,0)]))

s :: (Int, Int, Int) -> (Int, Int)
s (x,y,z) = (-z - x, y)

parser :: Parser [(Int, Int, Int)]
parser = discLine `sepEndBy1` newline

discLine :: Parser (Int,Int,Int)
discLine = do
    _ <- string "Disc #"
    n <- int
    _ <- string " has "
    m <- int
    _ <- string " positions; at time=0, it is at position "
    r <- int
    _ <- string "."
    return (n,m,r)
