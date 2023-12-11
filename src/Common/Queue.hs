{-# Language PatternSynonyms, ViewPatterns #-}
module Common.Queue (Queue(Queue, Empty, (:<|)), (|>), singleton, fromList, push, pop, appendList) where

import Data.Foldable (Foldable(..))

data Queue a = Queue [a] !Int

pattern Empty :: Queue a
pattern Empty = Queue [] 0

pattern (:<|) :: a -> Queue a -> Queue a
pattern x :<| xs <- (pop -> Just (x, xs))

infix 6 :<|

(|>) :: Queue a -> a -> Queue a
q |> x = push x q

singleton :: a -> Queue a
singleton x = Queue [x] 1

fromList :: [a] -> Queue a
fromList xs = Queue xs (length xs)

-- | Append multiple items to the end of the queue.
appendList :: Queue a -> [a] -> Queue a
appendList = foldl' (|>)

-- | Pop an element from the beginning of the queue.
pop :: Queue a -> Maybe (a, Queue a)
pop (Queue (x:f) _) = Just (x, fromList f)
pop _               = Nothing

-- | Add an element to the end of the queue.
push :: a -> Queue a -> Queue a
push x (Queue f s) = Queue (f ++ [x]) (s+1)