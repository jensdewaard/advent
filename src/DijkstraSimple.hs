module DijkstraSimple where

{- required packages:
   containers, unordered-containers, hashable
-}

import Data.Map.Strict as M
import Data.List as L
import Data.Array as A
import Data.Set as S
import Data.Heap as H
import Data.Graph
import Data.Maybe (fromMaybe)
import Data.Foldable as F

data Distance a = Dist a | Infinity
  deriving (Show, Eq)

instance (Ord a) => Ord (Distance a) where
  Infinity <= Infinity = True
  Infinity <= Dist x = False
  Dist x <= Infinity = True
  Dist x <= Dist y = x <= y

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

(!??) :: (Ord k, Eq k) => M.Map k (Distance d) -> k -> Distance d
(!??) distanceMap key = fromMaybe Infinity (M.lookup key distanceMap)

data DijkstraState = DijkstraState
  { visitedSet :: Set Int
  , distanceMap :: M.Map Int (Distance Int)
  , nodeQueue :: H.MinPrioHeap (Distance Int) Int
  }


findShortestDistance :: Graph -> Int -> Int -> Distance Int
findShortestDistance graph src dest = processQueue initialState !?? dest
  where
    initialVisited = S.empty
    initialDistances = M.singleton src (Dist 0)
    initialQueue = H.fromList [(Dist 0, src)]
    initialState = DijkstraState initialVisited initialDistances initialQueue

    processQueue :: DijkstraState -> M.Map Int (Distance Int)
    processQueue ds@(DijkstraState v0 d0 q0) = case H.view q0 of
      Nothing -> d0
      Just ((minDist, node), q1) -> if node == dest then d0
        else if S.member node v0 then processQueue (ds {nodeQueue = q1})
        else
          -- Update the visited set
          let v1 = S.insert node v0
          -- Get all unvisited neighbors of our current node
              allNeighbors = (A.!) graph node
              unvisitedNeighbors = L.filter (\(n, _) -> not (S.member n v1)) [(n, Dist 1) | n <- allNeighbors]
          -- Fold each neighbor and recursively process the queue
          in  processQueue $ L.foldl (foldNeighbor node) (DijkstraState v1 d0 q1) unvisitedNeighbors
    foldNeighbor current ds@(DijkstraState v1 d0 q1) (neighborNode, cost) =
      let altDistance = addDist (d0 !?? current) cost
      in  if altDistance < d0 !?? neighborNode
            then DijkstraState v1 (M.insert neighborNode altDistance d0) (H.insert (altDistance, neighborNode) q1) 
            else ds

contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (a:as) a' = (a == a') || contains as a'
