{-# LANGUAGE InstanceSigs #-}
module Common.Interval (Interval (..), Comparison (..), Common.Interval.compare, Common.Interval.length, distinct, singleton, fromList, fromPair, overlaps, union, includes, ub, lb, borders) where

import Data.Ord (comparing)

data Interval a = Empty | Interval a a

instance Eq a => Eq (Interval a) where
    (==) i j = lb i == lb j && ub i == ub j

instance Show a => Show (Interval a) where
  show :: Interval a -> String
  show Empty = "[]"
  show (Interval l u) = '[' : show l ++ "," ++ show u ++ "]"

instance Ord a => Ord (Interval a) where
    compare = comparing lb <> comparing ub

data Comparison = LT -- ^ The first interval is completely less than the second.
    | LTE  -- ^ The first interval overlaps with the beginning of the second.
    | GT  -- ^ The first interval is completely greater than the second.
    | GTE  -- ^ The first interval overlaps with the end of the second.
    | IN   -- ^ The first interval is a subset of the second.
    | OUT  -- ^ The first interval is a superset of the second.

compare :: Ord a => Interval a -> Interval a -> Comparison
compare i j
    | i `overlaps` j = LTE
    | j `overlaps` i = GTE
    | j `includes` i = IN
    | i `includes` j = OUT
    | ub i < lb j = Common.Interval.LT
    | ub j < lb i = Common.Interval.GT
    | otherwise = error "unable to compare intervals"

singleton :: a -> Interval a
singleton i = Interval i i

fromPair :: Ord a => (a, a) -> Interval a
fromPair (x, y) = Interval (min x y) (max x y)

fromList :: (Ord a) =>[a] -> Interval a
fromList is = Interval (minimum is) (maximum is)

length :: (Enum a) => Interval a -> Int
length Empty = 0
length (Interval l u) = Prelude.length [l..u]

lb :: Interval a -> a
lb Empty = error "lower bound of empty interval"
lb (Interval x _) = x

ub :: Interval a -> a
ub Empty = error "upper bound of empty interval"
ub (Interval _ y) = y

distinct :: Ord a => Interval a -> Interval a -> Bool
distinct i j = not (overlaps i j || overlaps j i)

overlaps :: (Ord a) =>Interval a -> Interval a -> Bool
overlaps Empty _ = False
overlaps _ Empty = False
overlaps (Interval lbp ubp) (Interval lbq ubq) = lbp <= lbq && lbq <= ubp && ubp <= ubq

union :: (Ord a) =>Interval a -> Interval a -> Interval a
union p Empty = p
union Empty q = q
union (Interval lbp ubp) (Interval lbq ubq) = fromPair (min lbp lbq, max ubp ubq)

includes :: (Ord a) =>Interval a -> Interval a -> Bool
includes _ Empty = False
includes Empty _ = False
includes (Interval lbp ubp) (Interval lbq ubq) = lbp <= lbq && ubq <= ubp

borders :: (Ord a, Num a) =>Interval a -> Interval a -> Bool
borders Empty _ = False
borders _ Empty = False
borders (Interval _ ubp) (Interval lbq _) = lbq - ubp == 1

instance Ord a => Semigroup (Interval a) where
    (<>) :: Interval a -> Interval a -> Interval a
    (<>) i j = fromPair (min (lb i) (lb j), max (ub i) (ub j))

instance Ord a => Monoid (Interval a) where
    mempty :: Interval a
    mempty = Empty