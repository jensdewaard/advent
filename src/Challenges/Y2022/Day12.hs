module Challenges.Y2022.Day12 (solutionA, solutionB) where

import Common.Coord (Coord, cardinal)
import Common.Prelude
import Common.Parsing (grid)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec ( letter, Parser )
import Data.Char ( ord )
import Common.Search (dijkstra)

solutionA :: String -> String
solutionA = parser ==> \m -> dijkstra
  (adjacentNodes m) -- adjacency
  [(startNode m, 0)]   -- 
  (+) -- combination
  (const 1) -- estimation
  (\c -> m Map.! c == 'E') --end state

solutionB :: String -> String
solutionB = parser ==> \m -> dijkstra 
  (adjacentNodes m)
  (map (\(c,_) -> (c,0)) $ filter (\(_,t) -> t == 'a') (Map.assocs m))
  (+) (const 1) (\c -> m Map.! c == 'E')

startNode :: Map a Char -> a
startNode m = fst $ head $ filter (\(_,t) -> t == 'S') (Map.assocs m)

parser :: Parser (Map Coord Char)
parser = grid letter

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
