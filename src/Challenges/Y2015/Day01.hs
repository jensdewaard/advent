module Challenges.Y2015.Day01 (input, solutionA, solutionB) where

import Data.List (findIndex)

input :: Bool -> IO String
input True = return "))((((("
input False = readFile "data/2015/01.txt"

solutionA :: String -> String
solutionA i = show $ foldl foldInput 0 i
solutionB :: String -> String
solutionB i = show $ findIndex (<0) $ scanl foldInput 0 i

foldInput :: Int -> Char -> Int
foldInput n '(' = n + 1
foldInput n ')' = n - 1
foldInput _ _ = error "undefined input"