{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Search (bfs, bfsUntil, dfs, dfsUntil, Cost (..), search, Dijkstra (..), dijkstra, dijkstraOn, simple, searchFor, searchFor') where

import Common.Queue as Queue
  ( Queue (Empty, (:<|)),
    appendList,
    fromList,
    (<|>),
  )
import Control.Monad (mplus)
import Data.Kind (Type)
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.PQueue.Min as PM
import qualified Data.Set as Set (empty, insert, member)

--
dfsUntil :: (Ord a) => (a -> Bool) -> (a -> [a]) -> [a] -> [a]
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

dfs :: (Ord a) => (a -> [a]) -> [a] -> [a]
dfs = dfsUntil (const False)

--- BFS
bfsUntil :: (Ord a) => (a -> Bool) -> (a -> [a]) -> [a] -> [a]
bfsUntil predicate next start = loop Set.empty (fromList start)
  where
    loop _ Empty = []
    loop visited (x :<| q)
      | predicate x = [x]
      | Set.member x visited = loop visited q
      | otherwise = x : loop v' q'
      where
        v' = Set.insert x visited
        q' = appendList q (next x)
    loop _ _ = error "cannot pop from non-empty queue"

bfs :: (Ord a) => (a -> [a]) -> [a] -> [a]
bfs = bfsUntil (const False)

class (Ord a) => Cost a where
  combine :: a -> a -> a
  nocost :: a

instance Cost Int where
  combine = (+)
  nocost = 0

-- data (Cost cost, Representable state b, Ord b) => Dijkstra state b cost = Dijkstra
--   { adjacency :: state -> [(state, cost)]
--   , estimate :: state -> cost
--   , target :: state -> Bool
--   }

class Dijkstra graph where
  type DijkstraCost graph :: Type
  type DijkstraRepr graph :: Type
  represent :: graph -> DijkstraRepr graph
  adjacency :: graph -> [(graph, DijkstraCost graph)]
  estimate :: graph -> DijkstraCost graph

searchFor ::
  forall g.
  (Dijkstra g, Cost (DijkstraCost g), Ord g, Ord (DijkstraRepr g)) =>
  (g -> Bool) ->
  [g] ->
  Maybe (g, DijkstraCost g)
searchFor = flip search

searchFor' ::
  forall g.
  (Dijkstra g, Cost (DijkstraCost g), Ord g, Ord (DijkstraRepr g)) =>
  (g -> Bool) ->
  [g] ->
  [(g, DijkstraCost g)]
searchFor' predicate starts =
  dijkstraOn' represent adjacency (map (,nocost) starts) combine estimate predicate

search ::
  forall g.
  (Dijkstra g, Cost (DijkstraCost g), Ord g, Ord (DijkstraRepr g)) =>
  [g] ->
  (g -> Bool) ->
  Maybe (g, DijkstraCost g)
search start = dijkstraOn represent adjacency (map (,nocost) start) combine estimate

dijkstraOn' ::
  (Ord state, Ord b, Ord cost) =>
  (state -> b) -> -- state representation function
  (state -> [(state, cost)]) -> -- adjacency function with weights
  [(state, cost)] -> -- starts
  (cost -> cost -> cost) -> -- function for combining weights
  (state -> cost) -> -- estimation function
  (state -> Bool) -> -- acceptance function
  [(state, cost)]
dijkstraOn' repr trans start combine estimate accept =
  go M.empty (foldl' (\hp (a, b) -> PM.insert (b, b, a) hp) PM.empty start)
  where
    go !seen heap = case PM.minView heap of
      Nothing -> []
      Just ((_, cost, state), heap')
        | accept state -> (state, cost) : go seen heap'
        | repr state `M.member` seen && cost > fromJust (M.lookup (repr state) seen) ->
            go seen heap'
        | otherwise ->
            let nxts =
                  ( \(val', cost') ->
                      let ncost = combine cost cost'
                       in (combine (estimate val') ncost, ncost, val')
                  )
                    <$> trans state
                heap'' = foldl' (flip PM.insert) heap' nxts
             in go (M.insert (repr state) cost seen) heap''

dijkstraOn ::
  (Ord state, Ord b, Ord cost) =>
  (state -> b) -> -- state representation function
  (state -> [(state, cost)]) -> -- adjacency function with weights
  [(state, cost)] -> -- starts
  (cost -> cost -> cost) -> -- function for combining weights
  (state -> cost) -> -- estimation function
  (state -> Bool) -> -- acceptance function
  Maybe (state, cost)
dijkstraOn repr trans start combine estimate accept =
  go Set.empty (foldl' (\hp (a, b) -> PM.insert (b, b, a) hp) PM.empty start)
  where
    go !seen heap = case PM.minView heap of
      Nothing -> Nothing
      Just ((_, cost, state), heap')
        | accept state -> Just (state, cost)
        | repr state `Set.member` seen -> go seen heap'
        | otherwise ->
            let nxts =
                  ( \(val', cost') ->
                      let ncost = combine cost cost'
                       in (combine (estimate val') ncost, ncost, val')
                  )
                    <$> trans state
                heap'' = foldl' (flip PM.insert) heap' nxts
             in go (Set.insert (repr state) seen) heap''

dijkstra ::
  (Ord state, Ord cost) =>
  (state -> [(state, cost)]) -> -- adjacency function with weights
  [(state, cost)] -> -- starts
  (cost -> cost -> cost) -> -- function for combining weights
  (state -> cost) -> -- estimation function
  (state -> Bool) -> -- acceptance function
  Maybe (state, cost)
dijkstra = dijkstraOn id

simple ::
  (Ord state) =>
  (state -> [state]) ->
  state ->
  (state -> Bool) ->
  Maybe (state, Int)
simple next start = dijkstra (map (,1) . next) [(start, 0)] (+) (const 1)

binary :: [a] -> (a -> Bool) -> Maybe a
binary [] _ = Nothing
binary [a] target = if target a then Just a else Nothing
binary as target =
  let n = length as `div` 2
      l = binary (take n as) target
      r = binary (drop n as) target
   in mplus l r
