module Challenges.Y2019.Day01 (solutionA, solutionB) where

import Text.ParserCombinators.Parsec
import Common.Prelude
import Parsing (int)

fuelRequired :: Int -> Int
fuelRequired m = (m `div` 3) - 2

fuelRequiredR :: Int -> Int
fuelRequiredR m
    | m <= 0 = 0
    | otherwise = m' + m''
        where
            m' = fuelRequired m
            m'' = max (fuelRequiredR m') 0

solutionA :: String -> String
solutionA = solve (int `sepEndBy1` newline) (sum . map fuelRequired)

solutionB :: String -> String
solutionB = solve (int `sepEndBy1` newline) (sum .map fuelRequiredR)