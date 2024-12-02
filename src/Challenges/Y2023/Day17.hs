{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
module Challenges.Y2023.Day17 (solutionA, solutionB) where
import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec hiding (State)
import Common.Prelude ( solve )
import Data.List (singleton)
import Data.Map (Map)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as M
import Common.Coord (Coord, Dir (..), reverse, cardinal, direction)
import Common.Parsing (grid)
import Prelude hiding (reverse)
import Common.Search (Dijkstra(..), searchFor)
import Control.Arrow ((>>>))
import Data.Ord (comparing)

solutionA :: String -> String
-- solutionA = solve parser (\m -> dijkstra
--     (next m) [(State (1,1) D, 0)] (+) estimate (==State (13,13) D))
solutionA = solve parser (State (1,1) [] [] >>> singleton >>> searchFor destination >>> fromJust >>> \(s,i) -> (path s, i))
solutionB :: String -> String
solutionB = solve parser (const "")

data State = State
  { coords :: Coord
  , path :: [Dir]
  , heats :: [Int]
  , world :: Map Coord Int
  } deriving Show

destination :: State -> Bool
destination = (==(141,141)) . coords
-- destination :: State -> Bool
-- destination = (==(13,13)) . coords

instance Dijkstra State where
  type DijkstraCost State = Int
  type DijkstraRepr State = (Coord, [Dir])
  represent s = (coords s, take 3 $ path s)
  adjacency = next
  estimate (State c _ _ m) = fromJust $ M.lookup c m


-- estimate :: State -> Int
-- estimate (State c _ _) = dist c (13,13)

next :: State -> [(State, Int)]
next (State c ds hs m) = [ (State c' (dirTaken : ds) (cost : hs) m, cost) |
    c' <- cardinal c,
    let heat = M.lookup c' m,
    isJust heat,
    let cost = fromJust heat,
    let dirTaken = direction c c',
    null ds || any (/=dirTaken) (take 3 ds),
    null ds || dirTaken /= reverse (head ds)
  ]

nextB :: State -> [(State, Int)]
nextB (State c ds hs m) = [ (State c' (dirTaken : ds) (cost : hs) m, cost) |
    c' <- cardinal c,
    let heat = M.lookup c' m,
    isJust heat,
    let cost = fromJust heat,
    let dirTaken = direction c c',
    null ds || any (/=dirTaken) (take 10 ds),
    null ds || dirTaken /= reverse (head ds)
  ]

instance Eq State where (State c1 _ _ _) == (State c2 _ _ _) = c1 == c2
instance Ord State where compare = comparing (sum . heats) <> comparing (length . path)

parser :: Parser (Map Coord Int)
parser = grid (digitToInt <$> digit)