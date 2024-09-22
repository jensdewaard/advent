{-# LANGUAGE InstanceSigs #-}
module Common.Interval (Interval (..), Comparison (..), Common.Interval.compare, Common.Interval.length, distinct, singleton, fromList, fromPair, overlaps, union, includes, ub, lb, remove, removeAll, simplify, isIn) where

import Data.Ord (comparing)
import Data.List (sort)

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
    | EQ   -- ^ The intervals are equal

compare :: Ord a => Interval a -> Interval a -> Comparison
compare i j
    | lb i == lb j && ub i == ub j = Common.Interval.EQ
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

-- | The lower bound of an interval
lb :: Interval a -> a
lb Empty = error "lower bound of empty interval"
lb (Interval x _) = x

-- | The upper bound of an interval
ub :: Interval a -> a
ub Empty = error "upper bound of empty interval"
ub (Interval _ y) = y

-- | True if and only if the two given intervals have no overlapping numbers
distinct :: Ord a => Interval a -> Interval a -> Bool
distinct i j = not (overlaps i j || overlaps j i)

-- | True if there exist a number that is included in both intervals
overlaps :: (Ord a) =>Interval a -> Interval a -> Bool
overlaps Empty _ = False
overlaps _ Empty = False
overlaps (Interval lbp ubp) (Interval lbq ubq) = lbp <= lbq && lbq <= ubp && ubp <= ubq

-- | Returns an interval that includes all values of the two given intervals and any values in between.
union :: (Ord a) =>Interval a -> Interval a -> Interval a
union p Empty = p
union Empty q = q
union (Interval lbp ubp) (Interval lbq ubq) = fromPair (min lbp lbq, max ubp ubq)

-- | i `includes` j is true iff all values in j are included in i.
includes :: (Ord a) =>Interval a -> Interval a -> Bool
includes _ Empty = False
includes Empty _ = False
includes (Interval lbp ubp) (Interval lbq ubq) = lbp <= lbq && ubq <= ubp

isIn :: (Ord a) => a -> Interval a -> Bool
isIn x i = i `includes` singleton x

instance Ord a => Semigroup (Interval a) where
    (<>) :: Interval a -> Interval a -> Interval a
    (<>) Empty j = j
    (<>) i Empty = i
    (<>) i j = fromPair (min (lb i) (lb j), max (ub i) (ub j))

instance Ord a => Monoid (Interval a) where
    mempty :: Interval a
    mempty = Empty

-- | i `remove` j returns all values of i that are not included in j.
remove :: (Enum a, Ord a) => Interval a -> Interval a -> [Interval a]
remove i j = case i `Common.Interval.compare` j of
    LTE -> [ fromPair (lb i, pred $ lb j ) ]
    GTE -> [ fromPair (succ $ ub j, ub i) ]
    Common.Interval.LT -> [i]
    Common.Interval.GT -> [i]
    IN -> [ ]
    OUT -> [ fromPair (lb i , pred $ lb j), fromPair (succ $ ub j, ub i)]
    Common.Interval.EQ -> [ ]

-- | is `removeAll` j  removes all values in j from all intervals in is
removeAll :: (Enum a, Ord a) => [Interval a] -> Interval a -> [Interval a]
removeAll is j = simplify $ concatMap (`remove` j) is

simplify :: (Ord a, Enum a) => [Interval a] -> [Interval a]
simplify is = let
    is' = sort is
    simplify' :: (Enum a, Ord a) => [Interval a] -> [Interval a]
    simplify' (Empty : xs) = simplify' xs
    simplify' [] = []
    simplify' [i] = [i]
    simplify' (i:j:xs) = case Common.Interval.compare i j of
                                LTE -> simplify' ( fromPair (lb i, ub j) : xs)
                                Common.Interval.LT ->
                                    if ub i == pred (lb j)
                                       then simplify' (fromPair (lb i, ub j) : xs)
                                       else i : simplify' (j:xs)
                                Common.Interval.GT -> j : simplify' (i:xs)
                                GTE -> simplify' ( fromPair (lb j, ub i) : xs)
                                IN -> simplify' (j:xs)
                                OUT -> simplify' (i:xs)
                                Common.Interval.EQ -> simplify' (j:xs)
    in simplify' is'



















