module Challenges.Y2015.Day25 (solutionA, solutionB) where
import Common.Prelude
import Text.ParserCombinators.Parsec

solutionA :: String -> String
solutionA = solve parser (\(r,c) -> iterate step 20151125 !! index r c)
solutionB :: String -> String
solutionB = solve parser (const "Fin.")

index :: Int -> Int -> Int
index row col  = (row + col - 1) * (row + col - 2) `div` 2 + col - 1

step :: Int -> Int
step x = (x * 252533) `mod` 33554393

parser :: Parser (Int, Int)
parser = return (3010, 3019)
