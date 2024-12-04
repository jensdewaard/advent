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
import Common.Coord (Coord, Dir (..), reverse, cardinal, direction, moveN)
import Common.Parsing (grid)
import Prelude hiding (reverse)
import Common.Search (Dijkstra(..), searchFor)
import Control.Arrow ((>>>))
import Data.Ord (comparing)
import qualified Data.List as L
import Data.Bifunctor (first)

solutionA :: String -> String
-- solutionA = solve parser (\m -> dijkstra
--     (next m) [(State (1,1) D, 0)] (+) estimate (==State (13,13) D))
solutionA = solve parser (State (1,1) [] [] >>> singleton >>> searchFor destination >>> fromJust >>> snd)
solutionB :: String -> String
solutionB = solve parser (StateB (1,1) [] [] >>> singleton >>> searchFor destinationB >>> fromJust >>> first (L.reverse . pathB))

data State = State
  { coords :: Coord
  , path :: [Dir]
  , heats :: [Int]
  , world :: Map Coord Int
  } deriving Show

data StateB = StateB
  { coordsB :: Coord
  , pathB :: [Dir]
  , heatsB :: [Int]
  , worldB :: Map Coord Int
  } deriving Show
-- type StateB = State

destination :: State -> Bool
destination = (==(141,141)) . coords
-- destination :: State -> Bool
-- destination = (==(12,5)) . coords

destinationB :: StateB -> Bool
destinationB = (==(141,141)) . coordsB
-- destinationB :: StateB -> Bool
-- destinationB = (==(12,5)) . coordsB

instance Dijkstra State where
  type DijkstraCost State = Int
  type DijkstraRepr State = (Coord, [Dir])
  represent s = (coords s, take 3 $ path s)
  adjacency = next
  estimate (State c _ _ m) = fromJust $ M.lookup c m

instance Dijkstra StateB where
  type DijkstraCost StateB = Int
  type DijkstraRepr StateB = (Coord, [Dir])
  represent s = (coordsB s, take 10 $ pathB s)
  adjacency = nextB
  estimate (StateB c _ _ m) = fromJust $ M.lookup c m


-- estimate :: State -> Int
-- estimate (State c _ _) = dist c (13,13)

next :: State -> [(State, Int)]
next (State c ds hs m) = [ (State c' (dirTaken : ds) (cost : hs) m, cost) |
    c' <- cardinal c,
    let heat = M.lookup c' m,
    isJust heat,
    let cost = fromJust heat,
    let dirTaken = direction c c',
    null ds || any (/=dirTaken) (take 3 ds),   -- move max 3 steps in a direction
    null ds || dirTaken /= reverse (head ds)   -- do not reverse course
  ]

nextB :: StateB -> [(StateB, Int)]
nextB (StateB c ds hs m) = [ (StateB newP (replicate pathLength dirTaken ++ ds) (map fromJust pathHeats ++ hs) m, cost) |
    c' <- cardinal c,
    let dirTaken = direction c c',
    null ds || dirTaken /= reverse (head ds),  -- do not reverse course
    null ds || any (/=dirTaken) (take (10) ds), -- move max 10 steps in a direction
    pathLength <- [4..10],

    let newP = moveN pathLength dirTaken c,
    let pathHeats = map (\steps -> M.lookup (moveN steps dirTaken c) m) [1..pathLength],
    all isJust pathHeats,                     -- all travelled coords are in the map
    let cost = sum $ map fromJust pathHeats
  ]

instance Eq State where (State c1 _ _ _) == (State c2 _ _ _) = c1 == c2
instance Ord State where compare = comparing (sum . heats) <> comparing (length . path)
instance Eq StateB where (StateB c1 _ _ _) == (StateB c2 _ _ _) = c1 == c2
instance Ord StateB where compare = comparing (sum . heatsB) <> comparing (length . pathB)

parser :: Parser (Map Coord Int)
parser = grid (digitToInt <$> digit)