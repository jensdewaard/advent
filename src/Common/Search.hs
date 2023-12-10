{-# Language BangPatterns #-}
{-# Language LambdaCase #-}

module Common.Search (bfsN, bfsOnN) where

import Data.Set as Set
import Common.Queue as Queue

--- BFS
bfsN :: Ord a => (a -> [a]) -> [a] -> [a]
bfsN = bfsOnN id

-- | Generalization of 'bfsOn' allowing multiple
-- initial states to be considered.
bfsOnN ::
  Ord r =>
  (a -> r)   {- ^ representative function   -} ->
  (a -> [a]) {- ^ successor state generator -} ->
  [a]        {- ^ initial states            -} ->
  [a]        {- ^ reachable states          -}
bfsOnN rep next start = loop Set.empty (Queue.fromList start)
  where
    loop !seen = \case
      Queue.Empty -> []
      x Queue.:<| q
        | Set.member r seen ->     loop seen  q
        | otherwise         -> x : loop seen' q'
        where
          r     = rep x
          seen' = Set.insert r seen
          q'    = Queue.appendList q (next x)