module Challenges.Y2023.Day01 where
import Text.ParserCombinators.Parsec

import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (fromJust)

import Shared (solve)

solutionA :: String -> String
solutionA = solve parseInput sum
solutionB :: String -> String
solutionB = undefined

parseInput :: Parser [Int]
parseInput = sepBy1 parseLine newline

parseLine :: Parser Int
--parseLine l = read $ (firstDigit l ++ lastDigit l)
parseLine = do
    l <- many1 alphaNum
    return $ read $ ([firstDigit l , lastDigit l])

firstDigit :: [Char] -> Char
firstDigit = fromJust . find isDigit
lastDigit :: [Char] -> Char
lastDigit = fromJust . (find isDigit) . reverse