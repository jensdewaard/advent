{-# LANGUAGE BangPatterns #-}
module Common.Search (bfs, bfsUntil, dfsUntil, dijkstra) where

import qualified Data.PQueue.Min as PM
import qualified Data.Set as Set ( empty, insert, member )
import Data.List ( foldl' )
import Common.Queue as Queue
    ( Queue( (:<|), Empty), pop, fromList, appendList, (<|>), (|>))

--
dfsUntil :: Ord a => (a -> Bool) -> (a -> [a]) -> [a] -> [a]
dfsUntil predicate next start = loop Set.empty (fromList start)
  where
    loop _ Empty = []
    loop visited (x :<| q)
      | predicate x = [x]
      | Set.member x visited = loop visited q
      | otherwise = x : loop v' q'
        where
          v' = Set.insert x visited
          q' = fromList (next x) <|> q
    loop _ _ = error "cannot pop from non-empty queue"

--- BFS
bfsUntil :: Ord a => (a -> Bool) -> (a -> [a]) -> [a] -> [a]
bfsUntil predicate next start = loop Set.empty (fromList start)
  where
    loop  _ Empty = []
    loop visited (x :<| q)
      | predicate x = [x]
      | Set.member x visited = loop visited q
      | otherwise         = x : loop v' q'
      where
        v' = Set.insert x visited
        q'    = appendList q (next x)
    loop _ _ = error "cannot pop from non-empty queue"
bfs :: Ord a => (a -> [a]) -> [a] -> [a]
bfs = bfsUntil (const False)

dijkstra :: (Ord state, Ord cost) =>
  (state -> [(state,cost)]) -- adjacency function with weights
  -> [(state, cost)] -- starts
  -> (cost -> cost -> cost) -- function for combining weights
  -> (state -> cost) -- estimation function
  -> (state -> Bool) -- acceptance function
  -> Maybe (state, cost)
dijkstra trans start combo estimate accept =
  go Set.empty (foldl' (\hp (a,b) -> PM.insert (b,b,a) hp) PM.empty start)
  where
    go !seen heap = case PM.minView heap of
      Nothing -> Nothing
      Just ((_,cost,value), heap')
        | value `Set.member` seen -> go seen heap'
        | accept value -> Just (value, cost)
        | otherwise ->
            let nxts = (\(val', cost') -> let ncost = combo cost cost' in 
                  (combo (estimate val') ncost, ncost, val')) <$> trans value
                heap'' = foldl' (flip PM.insert) heap' nxts
             in go (Set.insert value seen) heap''