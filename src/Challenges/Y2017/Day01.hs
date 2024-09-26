module Challenges.Y2017.Day01 (solutionA, solutionB) where

import Common.List (sumWith)
import Common.Prelude
import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec

solutionA :: String -> String
solutionA = solve parser (sumWith digitToInt . matches . cycleA)

solutionB :: String -> String
solutionB = solve parser (sumWith digitToInt . uncurry matchesB . cycleB)

parser :: Parser [Char]
parser = many1 digit

cycleA :: [a] -> [a]
cycleA [] = []
cycleA (a : as) = [a] ++ as ++ [a]

cycleB :: [a] -> ([a], [a])
cycleB as =
  let half = length as `div` 2
      front = take half as
      back = drop half as
   in (front ++ back, back ++ front)

matches :: (Eq a) => [a] -> [a]
matches [] = []
matches [_] = []
matches (a : b : cs) = ([a | a == b]) ++ matches (b : cs)

matchesB :: (Eq a) => [a] -> [a] -> [a]
matchesB [] _ = []
matchesB _ [] = []
matchesB (a : as) (b : bs) = ([a | a == b]) ++ matchesB as bs
