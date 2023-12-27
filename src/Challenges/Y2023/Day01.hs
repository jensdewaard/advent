module Challenges.Y2023.Day01 where
import Text.ParserCombinators.Parsec

import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Text (pack, unpack, replace)

import Common.Prelude

solutionA :: String -> String
solutionA = solve parseInput sum
solutionB :: String -> String
solutionB = solve parseInput' sum

parseInput :: Parser [Int]
parseInput = sepBy1 parseLine newline
parseInput' = sepBy1 parseLine' newline

parseLine :: Parser Int
--parseLine l = read $ (firstDigit l ++ lastDigit l)
parseLine = do
    l <- many1 alphaNum
    return $ read $ ([firstDigit l , lastDigit l])

parseLine' :: Parser Int
parseLine' = do
    l <- many1 alphaNum
    let l' = repDigits l
    return $ read $ ([firstDigit l', lastDigit l'])

firstDigit :: [Char] -> Char
firstDigit = fromJust . find isDigit
lastDigit :: [Char] -> Char
lastDigit = fromJust . (find isDigit) . reverse

replace' :: String -> String -> String -> String
replace' t w = unpack . (replace (pack t) (pack w)) . pack  

repDigits :: String -> String
repDigits = replace' "one" "one1one" . 
    replace' "two" "two2two" . 
    replace' "three" "three3three" . replace' "four" "four4four" .
    replace' "five" "five5five" . replace' "six" "six6six" . 
    replace' "seven" "seven7seven" . replace' "eight" "eight8eight" .
    replace' "nine" "nine9nine"