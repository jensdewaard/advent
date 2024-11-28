{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Challenges.Y2022.Day12 (solutionA, solutionB) where

import Common.Coord (Coord, cardinal)
import Common.Prelude
import Common.Parsing (grid)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec ( letter, Parser )
import Data.Char ( ord )
import Common.Search (dijkstra, Dijkstra (..), search)

solutionA :: String -> String
-- solutionA = parser ==> \m -> dijkstra
--   (adjacentNodes m) -- adjacency
--   [(startNode m, 0)]   -- 
--   (+) -- combination
--   (const 1) -- estimation
--   (\c -> m Map.! c == 'E') --end state
solutionA = solve (grid letter) (\x -> search [startNode x] (\c -> x Map.! c == 'E') x)

solutionB :: String -> String
solutionB = solve (grid letter) (\x -> search (allStarts x) (\c -> x Map.! c == 'E') x)
-- solutionB = solve (grid letter) (\m -> dijkstra
--   (adjacentNodes m)
--   (map (\(c,_) -> (c,0)) $ filter (\(_,t) -> t == 'a') (Map.assocs m))
--   (+) (const 1) (\c -> m Map.! c == 'E'))

allStarts :: Map Coord Char -> [Coord]
allStarts m = map fst $ filter (\(_,t) -> t == 'a') (Map.assocs m)

startNode :: Map a Char -> a
startNode m = fst $ head $ filter (\(_,t) -> t == 'S') (Map.assocs m)

instance Dijkstra (Map Coord Char) where
  type DijkstraCost (Map Coord Char) = Int
  type DijkstraNode (Map Coord Char) = Coord
  type DijkstraRepr (Map Coord Char) = Coord
  represent :: Map Coord Char -> DijkstraNode (Map Coord Char) -> DijkstraRepr (Map Coord Char)
  represent _ = id
  adjacency :: Map Coord Char -> DijkstraNode (Map Coord Char)
    -> [(DijkstraNode (Map Coord Char), DijkstraCost (Map Coord Char))]
  adjacency = adjacentNodes
  estimate :: Map Coord Char -> DijkstraNode (Map Coord Char) -> DijkstraCost (Map Coord Char)
  estimate _ = const 1

adjacentNodes :: Map Coord Char -> Coord -> [(Coord, Int)]
adjacentNodes m c = [(c', 1) | c' <- cardinal c,
    Map.member c' m,
    hdiff (m Map.! c) (m Map.! c') <= 1
  ]

hdiff :: Char -> Char -> Int
hdiff 'S' b = hdiff 'a' b
hdiff 'E' b = hdiff 'z' b
hdiff a 'S' = hdiff a 'a'
hdiff a 'E' = hdiff a 'z'
hdiff a b = ord b - ord a
