{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
module Challenges.Y2019.Day15 (solutionA, solutionB) where
import Common.Prelude
import Common.Coord (Coord, above, below, left, right, cardinal)
import Intcode (ProgState (inputs, outputs), runInterpreterUntilOutput, parseProgram, OpProgram, mkProgram)
import Common.Search (Dijkstra (..), search, bfs)
import Data.Function (on)
import Data.Bifunctor (Bifunctor (first))
import Data.Maybe (fromJust)

solutionA :: String -> String
solutionA = solve parseProgram (\p -> first position $ fromJust $  search [initState p] ((==2) . status) (initState p))
solutionB :: String -> String
solutionB = solve parseProgram (\p ->
    let
        reached = map (position . fst) $ bfs (nextStates . fst) [(DroidState (0,0) (mkProgram p) 0, 0)]
        rewalked = bfs (adjWithDepth reached) [DroidDepth ((-14,16), 0)]
    in rewalked)

newtype Depth = DroidDepth (Coord, Int) deriving Show

instance Eq Depth where
    (==) (DroidDepth (d1,_)) (DroidDepth (d2,_)) = d1 == d2
instance Ord Depth where
  compare (DroidDepth (d1,_)) (DroidDepth (d2,_))= compare d1 d2

instance Dijkstra DroidState where
  type DijkstraCost DroidState = Int
  type DijkstraNode DroidState = DroidState
  type DijkstraRepr DroidState = Coord
  represent :: DroidState -> DijkstraNode DroidState -> DijkstraRepr DroidState
  represent = const position
  adjacency :: DroidState -> DijkstraNode DroidState -> [(DijkstraNode DroidState, DijkstraCost DroidState)]
  adjacency = const nextStates
  estimate :: DroidState -> DijkstraNode DroidState -> DijkstraCost DroidState
  estimate _ = const 1

initState :: OpProgram -> DroidState
initState p = DroidState (0,0) (mkProgram p) 0

data DroidState = DroidState
    {   position :: Coord
    ,   brain    :: ProgState
    ,   status   :: Int
    } deriving (Eq, Show)

instance Ord DroidState where
  compare = compare `on` position

run :: DroidState -> Int -> DroidState
run (DroidState p b _) i = let
        b' = runInterpreterUntilOutput $ b { inputs = [i], outputs = [] }
        state = head $ outputs b'
        p' = if state == 0 then p else move i p
        in DroidState p' b' state

adjWithDepth :: [Coord] -> Depth -> [Depth]
adjWithDepth reached (DroidDepth (c,n)) = 
    map (\x -> DroidDepth (x,n+1)) $ filter (`elem` reached) $ cardinal c

move :: Int -> Coord -> Coord
move 1 = above
move 2 = below
move 3 = left
move 4 = right
move _ = error "invalid move"


nextStates :: DroidState -> [(DroidState, Int)]
nextStates ds = map ((,1) . run ds) [1,2,3,4]