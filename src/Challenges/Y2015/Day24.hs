module Challenges.Y2015.Day24 (solutionA, solutionB) where
import Text.ParserCombinators.Parsec
import Common.Prelude
import Common.Parsing (int)
import Data.List (permutations)
import Common.List (takeUntil)

solutionA :: String -> String
solutionA = solve parser (group3)
solutionB :: String -> String
solutionB = solve parser (const "")

group3 :: [Int] -> [([Int], [Int], [Int])]
group3 presents = let groupTotal = sum presents `div` 3 in [(one,two,three) |
        ps <- permutations presents,
        let one = takeUntil ((>= groupTotal) . sum) ps,
        sum one == groupTotal,
        let ps' = drop (length one) ps,
        let two = takeUntil ((>= groupTotal) . sum) ps',
        sum two == groupTotal,
        let three = drop (length two) ps'
    ]

parser :: Parser [Int]
parser = int `sepEndBy1` newline
