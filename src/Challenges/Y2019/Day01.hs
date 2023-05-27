module Challenges.Y2019.Day01 where

import qualified Data.List as L

fuelRequired :: Int -> Int
fuelRequired m = (m `div` 3) - 2

fuelRequiredR :: Int -> Int
fuelRequiredR m
    | m <= 0 = 0
    | otherwise = m' + m''
        where
            m' = fuelRequired m
            m'' = max (fuelRequiredR m') 0


input :: Bool -> IO String
input _ = readFile "data/2019/01.txt"

solutionA :: String -> String
solutionA inp = show $ sum $ map (fuelRequired . readInt) (L.lines inp)

solutionB :: String -> String
solutionB inp = show $ sum $ map (fuelRequiredR . readInt) (L.lines inp)

readInt :: String -> Int
readInt = read