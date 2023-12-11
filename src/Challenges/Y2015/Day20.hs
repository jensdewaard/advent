module Challenges.Y2015.Day20 (solutionA, solutionB) where
import Shared (solve)
import Parsing (int)
import Data.Map (Map, toList)
import qualified Data.Map as Map

solutionA :: String -> String
solutionA = solve int (\x -> firstWhere (sndAbove x) $ toList $ updateMap (x `div` 10) Map.empty)
solutionB :: String -> String
solutionB = solve int (\x -> firstWhere (sndAbove x) $ toList $ updateMapB (x `div` 10) Map.empty)

sndAbove :: Int -> (Int, Int) -> Bool
sndAbove t (_,y) = y >= t

firstWhere :: (a -> Bool) -> [a] -> a
firstWhere _ [] = error "first on empty list"
firstWhere predicate (l:ls) = if predicate l then l else firstWhere predicate ls

updateMap :: Int -> Map Int Int -> Map Int Int
updateMap maxi m = foldl (updateMapForElf maxi) m [1..maxi]

updateMapB :: Int -> Map Int Int -> Map Int Int
updateMapB maxi m = foldl updateMapForElfB m [1..maxi]

updateMapForElf :: Int -> Map Int Int -> Int -> Map Int Int
updateMapForElf maxi m elf = foldl (updateMap' elf) m [elf, elf+elf..maxi]

updateMapForElfB :: Map Int Int -> Int -> Map Int Int
updateMapForElfB m elf = foldl (updateMapB' elf) m [elf, 2*elf..50*elf]

updateMap' :: Int -> Map Int Int -> Int -> Map Int Int
updateMap' n m i = Map.alter (Just . maybe (10*n) (+10*n)) i m

updateMapB' :: Int -> Map Int Int -> Int -> Map Int Int
updateMapB' n m i = Map.alter (Just . maybe (11*n) (+11*n)) i m