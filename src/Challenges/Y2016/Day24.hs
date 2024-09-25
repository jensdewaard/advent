module Challenges.Y2016.Day24 (solutionA, solutionB) where

import Common.Coord (Coord, cardinal)
import Common.Parsing (grid, symbol)
import Common.Prelude
import Common.Search (simple)
import Data.Char (digitToInt)
import qualified Data.Char as C
import Data.List (permutations)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, mapMaybe)
import Text.ParserCombinators.Parsec

solutionA :: String -> String
solutionA = solve parser (`solution` map (0 :) (permutations [1 .. 7]))

solutionB :: String -> String
solutionB = solve parser (`solution` map (\p -> 0 : p ++ [0]) (permutations [1 .. 7]))

solution :: Map Coord Char -> [[Int]] -> Int
solution m =
  let spots = mapMaybe (findChar m . C.intToDigit) [0 .. 7]
      pairs = [swing x y | x <- spots, y <- spots, x < y]
      neighF = neighbors m
      search (a, b) = simple neighF a (== b)
      minDists = map (\(x, y) -> (x, snd $ fromJust $ search y)) pairs
   in minimum . map (pathLength minDists)

pathLength :: [((Char, Char), Int)] -> [Int] -> Int
pathLength _ [] = 0
pathLength _ [_] = 0
pathLength minDists (a : b : ps) = lookupDist a b minDists + pathLength minDists (b : ps)
  where
    lookupDist :: Int -> Int -> [((Char, Char), Int)] -> Int
    lookupDist x y [] = error ("could not find distance between " ++ show x ++ " and " ++ show y)
    lookupDist x y (((n', m'), d) : ds) =
      if (digitToInt n' == x && digitToInt m' == y) || ((digitToInt n' == y) && digitToInt m' == x)
        then d
        else lookupDist x y ds

swing :: (Coord, Char) -> (Coord, Char) -> ((Char, Char), (Coord, Coord))
swing (c1, n1) (c2, n2) = ((n1, n2), (c1, c2))

neighbors :: Map Coord Char -> Coord -> [Coord]
neighbors m c = filter ((\mc -> isJust mc && (fromJust mc /= '#')) . flip Map.lookup m) $ cardinal c

findChar :: Map Coord Char -> Char -> Maybe (Coord, Char)
findChar m c = f $ Map.assocs $ Map.filter (== c) m
  where
    f :: [(Coord, Char)] -> Maybe (Coord, Char)
    f [] = Nothing
    f (a : _) = Just a

parser :: Parser (Map Coord Char)
parser = grid (letter <|> try symbol <|> digit)
