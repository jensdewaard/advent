module Common.Poset (Poset ((<=)), coveringRelation) where

import Common.List (none)
import Prelude hiding ((<=))

class (Eq a) => Poset a where
  (<=) :: a -> a -> Bool

coveringRelation :: [a] -> (a -> a -> Bool) -> a -> a -> Bool
coveringRelation allItems lt a b =
  none (\c -> lt a c && lt c b) allItems
