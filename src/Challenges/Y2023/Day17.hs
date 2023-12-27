{-# LANGUAGE InstanceSigs #-}
module Challenges.Y2023.Day17 (solutionA, solutionB) where
import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec hiding (State)
import Common.Prelude
import Data.List (delete)
import Data.Map (Map)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as Map
import Common.Coord (Coord, Dir (..), moveN, reverse, dist)
import Common.Parsing (grid)
import Prelude hiding (reverse)
import Common.Search (dijkstra)

solutionA :: String -> String
solutionA = solve parser (\m -> dijkstra
    (next m) [(State (1,1) D, 0)] (+) estimate (==State (13,13) D))
solutionB :: String -> String
solutionB = solve parser (const "")

data State = State Coord Dir deriving (Show, Ord)

estimate :: State -> Int
estimate (State c _) = dist c (13,13)

coordsOf :: State -> Coord
coordsOf (State c _) = c

next :: Map Coord Int -> State -> [(State, Int)]
next m (State c d) = [(s,cost) |
    let ds = delete d $ delete (reverse d) [U,D,L,R],
    let nds = [(n,d') | n <- [1,2,3], d' <- ds],
    s <- map (\(n,d') -> State (moveN n d' c) d) nds,
    let mcost = Map.lookup (coordsOf s) m,
    isJust mcost,
    let cost = fromJust mcost
    ]

instance Eq State where
  (==) :: State -> State -> Bool
  (State c1 _) == (State c2 _) = c1 == c2

parser :: Parser (Map Coord Int)
parser = grid (digitToInt <$> digit)