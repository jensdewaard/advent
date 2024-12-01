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
import Common.Search (Dijkstra (..), search)

solutionA :: String -> String
-- solutionA = parser ==> \m -> dijkstra
--   (adjacentNodes m) -- adjacency
--   [(startNode m, 0)]   -- 
--   (+) -- combination
--   (const 1) -- estimation
--   (\c -> m Map.! c == 'E') --end state
solutionA = solve (grid letter) (\x -> search [startNode x] (\(MazePosition _ c) -> x Map.! c == 'E'))

solutionB :: String -> String
solutionB = solve (grid letter) (\x -> search (allStarts x) (\(MazePosition _ c) -> x Map.! c == 'E'))
-- solutionB = solve (grid letter) (\m -> dijkstra
--   (adjacentNodes m)
--   (map (\(c,_) -> (c,0)) $ filter (\(_,t) -> t == 'a') (Map.assocs m))
--   (+) (const 1) (\c -> m Map.! c == 'E'))

allStarts :: Map Coord Char -> [MazePosition]
allStarts m = map (\(c,_) -> MazePosition m c) $ filter (\(_,t) -> t == 'a') (Map.assocs m)

startNode :: Map Coord Char -> MazePosition
startNode m = MazePosition m $ fst $ head $ filter (\(_,t) -> t == 'S') (Map.assocs m)

instance Dijkstra MazePosition where
  type DijkstraCost MazePosition = Int
  type DijkstraRepr MazePosition = Coord
  represent :: MazePosition -> DijkstraRepr MazePosition
  represent = position
  adjacency :: MazePosition -> [(MazePosition, DijkstraCost MazePosition)]
  adjacency = adjacentNodes
  estimate :: MazePosition -> DijkstraCost MazePosition
  estimate = const 1

data MazePosition = MazePosition
  { world :: Map Coord Char
  , position :: Coord
  }

instance Eq MazePosition where (==) m1 m2 = position m1 == position m2
instance Ord MazePosition where (<=) m1 m2 = position m1 <= position m2
instance Show MazePosition where show = show . position 

adjacentNodes :: MazePosition -> [(MazePosition, Int)]
adjacentNodes (MazePosition m c) = [(MazePosition m c', 1) | c' <- cardinal c,
    Map.member c' m,
    hdiff (m Map.! c) (m Map.! c') <= 1
  ]

hdiff :: Char -> Char -> Int
hdiff 'S' b = hdiff 'a' b
hdiff 'E' b = hdiff 'z' b
hdiff a 'S' = hdiff a 'a'
hdiff a 'E' = hdiff a 'z'
hdiff a b = ord b - ord a
