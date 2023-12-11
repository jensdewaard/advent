module Common.Search (bfs, bfsUntil, dfsUntil) where

import qualified Data.Set as Set ( empty, insert, member )
import Common.Queue as Queue
    ( Queue( (:<|), Empty), fromList, appendList, (<|>) )

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