module Challenges.Y2022.Day01 (solutionA, solutionB) where

import Data.List ( sortBy )
import Common.Prelude
import Common.Parsing (int)
import Text.ParserCombinators.Parsec

solutionA :: String -> String
solutionA = parseFile ==> partA
solutionB :: String -> String
solutionB = parseFile ==> partB

partA :: [Elf] -> Int
partA elves = maximum $ sortBy (flip compare) (map sum elves)

partB :: [Elf] -> Int
partB elves = sum . take 3 $ sortBy (flip compare) (map sum elves)

parseFile :: Parser [Elf]
parseFile = elf `sepEndBy1` newline where
    elf = int `endBy1` newline

type Elf = [Int]