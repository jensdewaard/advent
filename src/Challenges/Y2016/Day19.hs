{-# OPTIONS_GHC -Wno-unused-matches #-}
module Challenges.Y2016.Day19 (solutionA, solutionB) where
import Common.Prelude
import Common.Parsing (int)
import Common.Math (josephus, biggestPowerOf)

solutionA :: String -> String
solutionA = solve int josephus
solutionB :: String -> String
solutionB = solve int partB

partB :: Int -> Int
partB n = let
              pot = 3 * biggestPowerOf 3 n
          in if n == pot then n else n - (pot `div` 3)
