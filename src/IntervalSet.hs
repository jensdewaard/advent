module IntervalSet (Interval (Interval, Empty), IntervalSet, IntervalSet.length,distinct, singleton, fromList, fromPair, contains, overlaps, union, includes, add, merge, ub, lb) where

data Interval = Empty | Interval Int Int deriving (Show, Eq)

singleton :: Int -> Interval
singleton i = Interval i i

fromPair :: (Int, Int) -> Interval
fromPair (x, y) = Interval (min x y) (max x y)

fromList :: [Int] -> Interval
fromList is = Interval (minimum is) (maximum is)

length :: Interval -> Int
length Empty = 0
length (Interval l u) = u - l

lb :: Interval -> Int
lb Empty = error "lower bound of empty interval"
lb (Interval x y) = x

ub :: Interval -> Int
ub Empty = error "upper bound of empty interval"
ub (Interval x y) = y

type IntervalSet = [Interval]

distinct :: Interval -> Interval -> Bool
distinct i j = not $ (overlaps i j || overlaps j i)

contains :: Int -> IntervalSet -> Bool
contains i = foldl (\b iv -> b || containsI i iv) False where
    containsI _ Empty = False
    containsI i' (Interval lb ub) = lb <= i' && i' <= ub

overlaps :: Interval -> Interval -> Bool
overlaps Empty _ = False
overlaps _ Empty = False
overlaps (Interval lbp ubp) (Interval lbq ubq) = lbp <= lbq && lbq <= ubp && ubp <= ubq

union :: Interval -> Interval -> Interval
union p Empty = p
union Empty q = q
union (Interval lbp ubp) (Interval lbq ubq) = fromPair (min lbp lbq, max ubp ubq)

includes :: Interval -> Interval -> Bool
includes _ Empty = False
includes Empty _ = False
includes (Interval lbp ubp) (Interval lbq ubq) = lbp <= lbq && ubq <= ubp

add :: Interval -> IntervalSet -> IntervalSet
add i ss = sanitize $ add' i ss

merge :: IntervalSet -> IntervalSet -> IntervalSet 
merge = foldl (flip add)

add' :: Interval -> IntervalSet -> IntervalSet
add' Empty s = s
add' i [] = [i]
add' i (s : ss)
    | s `includes` i = s : ss
    | s `overlaps` i = union i s : ss
    | i `borders` s  = union i s : ss
    | otherwise      = s : add i ss

sanitize :: IntervalSet -> IntervalSet
sanitize = foldr add' []

borders :: Interval -> Interval -> Bool
borders Empty _ = False
borders _ Empty = False
borders (Interval _ ubp) (Interval lbq _) = lbq - ubp == 1