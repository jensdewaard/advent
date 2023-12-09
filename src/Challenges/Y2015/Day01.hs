module Challenges.Y2015.Day01 (solutionA, solutionB) where

import Data.List (findIndex)
import Data.Maybe (fromJust)

solutionA :: String -> String
solutionA i = show $ foldl foldInput 0 i
solutionB :: String -> String
solutionB i = show $ fromJust $ findIndex (<0) $ scanl foldInput 0 i

foldInput :: Int -> Char -> Int
foldInput n '(' = n + 1
foldInput n ')' = n - 1
foldInput _ _ = error "undefined input"