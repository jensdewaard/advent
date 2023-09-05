module Challenges.Y2015.Day10 (solutionA, solutionB) where

solutionA :: String -> String
solutionA input = show $ length $ (iterate playGame input) !! 40
solutionB :: String -> String
solutionB _ = ""

playGame :: String -> String
playGame "" = ""
playGame (d:ds) = d' ++ playGame ds' where
    d' = (show (1 + (length $ takeWhile (==d) ds))) ++ [d]
    ds' = dropWhile (==d) ds
