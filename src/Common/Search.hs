{-# LANGUAGE BangPatterns #-}
module Common.Search (bfs, bfsUntil, dfs, dfsUntil, dijkstra, dijkstraOn) where

import qualified Data.PQueue.Min as PM
import qualified Data.Set as Set ( empty, insert, member )
import Data.List ( foldl' )
import Common.Queue as Queue
    ( Queue( (:<|), Empty), fromList, appendList, (<|>))

--
dfsUntil :: Ord a => (a -> Bool) -> (a -> [a]) -> [a] -> [a]
dfsUntil predicate next start = loop Set.empty (fromList start)
  where
    loop _ Empty = mempty
    loop visited (x :<| q)
      | predicate x = pure x
      | Set.member x visited = loop visited q
      | otherwise = x : loop v' q'
        where
          v' = Set.insert x visited
          q' = fromList (next x) <|> q
    loop _ _ = error "cannot pop from non-empty queue"

dfs :: Ord a => (a -> [a]) -> [a] -> [a]
dfs = dfsUntil (const False)

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

dijkstraOn :: (Ord state, Ord b, Ord cost) =>
  (state -> b) -- state representation function
  -> (state -> [(state,cost)]) -- adjacency function with weights
  -> [(state, cost)] -- starts
  -> (cost -> cost -> cost) -- function for combining weights
  -> (state -> cost) -- estimation function
  -> (state -> Bool) -- acceptance function
  -> Maybe (state, cost)
dijkstraOn repr trans start combine estimate accept =
  go Set.empty (foldl' (\hp (a,b) -> PM.insert (b,b,a) hp) PM.empty start)
  where
    go !seen heap = case PM.minView heap of
      Nothing -> Nothing
      Just ((_,cost,state), heap')
        | accept state -> Just (state, cost)
        | repr state `Set.member` seen -> go seen heap'
        | otherwise ->
            let nxts = (\(val', cost') -> let ncost = combine cost cost' in
                  (combine (estimate val') ncost, ncost, val')) <$> trans state
                heap'' = foldl' (flip PM.insert) heap' nxts
             in go (Set.insert (repr state) seen) heap''

dijkstra :: (Ord state, Ord cost) =>
  (state -> [(state,cost)]) -- adjacency function with weights
  -> [(state, cost)] -- starts
  -> (cost -> cost -> cost) -- function for combining weights
  -> (state -> cost) -- estimation function
  -> (state -> Bool) -- acceptance function
  -> Maybe (state, cost)
dijkstra = dijkstraOn id
