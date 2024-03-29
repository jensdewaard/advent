module Challenges.Y2015.Day10 (solutionA, solutionB) where

solutionA :: String -> String
solutionA = solve 40
solutionB :: String -> String
solutionB = solve 50

solve :: Int -> String -> String
solve x s = show $ length $ iterate playGame s !! x

playGame :: String -> String
playGame "" = ""
playGame (d:ds) = d' ++ playGame ds' where
    d' = show (1 + length (takeWhile (==d) ds)) ++ [d]
    ds' = dropWhile (==d) ds
