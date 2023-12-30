module Challenges.Y2016.Day05 (solutionA, solutionB) where
import Common.Prelude
import Challenges.Y2015.Day04 (hash, fiveZeroes)
import Text.ParserCombinators.Parsec
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (digitToInt)

solutionA :: String -> String
solutionA = solve parser (\s -> snd $ iterate (search s) (0,"") !! 8)
solutionB :: String -> String
solutionB = solve parser (\s -> map snd . Map.toAscList $ snd $ iterate (search' s) (0, Map.empty) !! 100)

search :: String -> (Int, String) -> (Int, String)
search input (n, s)
    | fiveZeroes (input `hash` n) = (n+1, s ++ [input `hash` n !! 5])
    | otherwise = search input (n+1,s)


search' :: String -> (Int, Map Int Char) -> (Int, Map Int Char)
search' _ (n, m)
    | Map.size m == 8 = (n,m)
search' input (n, m) = let
    h = input `hash` n
    i = digitToInt (h !! 5)
    v = h !! 6
    in
    (if fiveZeroes h && Map.notMember i m && i >= 0 && i < 8 
        then (n+1, Map.insert i v m) 
        else search' input (n+1, m))



parser :: Parser String
parser = many1 letter