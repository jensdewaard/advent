module Common.Search (bfs) where

import qualified Data.Set as Set ( empty, insert, member )
import Common.Queue as Queue
    ( Queue( (:<|), Empty), fromList, appendList )

--- BFS
bfs :: Ord a => (a -> [a]) -> [a] -> [a]
bfs next start = loop Set.empty (fromList start)
  where
    loop _ Empty = []
    loop visited (x :<| q)
      | Set.member x visited = loop visited q
      | otherwise         = x : loop v' q'
      where
        v' = Set.insert x visited
        q'    = appendList q (next x)
    loop _ _ = error "cannot pop from non-empty queue"